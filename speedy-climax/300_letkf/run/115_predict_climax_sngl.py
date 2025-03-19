import os

import numpy as np
import torch
from torchvision.transforms import transforms

import click

from datetime import datetime, timedelta

from climax.arch import ClimaX
from climax.utils.pos_embed import interpolate_pos_embed


DEFAULT_VARS = [
    "u_component_of_wind_925",
    "u_component_of_wind_850",
    "u_component_of_wind_700",
    "u_component_of_wind_600",
    "u_component_of_wind_500",
    "u_component_of_wind_250",
    "u_component_of_wind_50",
    "v_component_of_wind_925",
    "v_component_of_wind_850",
    "v_component_of_wind_700",
    "v_component_of_wind_600",
    "v_component_of_wind_500",
    "v_component_of_wind_250",
    "v_component_of_wind_50",
    "temperature_925",
    "temperature_850",
    "temperature_700",
    "temperature_600",
    "temperature_500",
    "temperature_250",
    "temperature_50",
    "specific_humidity_925",
    "specific_humidity_850",
    "specific_humidity_700",
    "specific_humidity_600",
    "specific_humidity_500",
    "specific_humidity_250",
    "specific_humidity_50",
    "geopotential_925",
    "geopotential_850",
    "geopotential_700",
    "geopotential_600",
    "geopotential_500",
    "geopotential_250",
    "geopotential_50",
    "2m_temperature",
    "10m_u_component_of_wind",
    "10m_v_component_of_wind",
    "land_sea_mask",
    "orography",
    "lattitude",
]

OUTPUT_VARS = [
    "u_component_of_wind_925",
    "u_component_of_wind_850",
    "u_component_of_wind_700",
    "u_component_of_wind_600",
    "u_component_of_wind_500",
    "u_component_of_wind_250",
    "u_component_of_wind_50",
    "v_component_of_wind_925",
    "v_component_of_wind_850",
    "v_component_of_wind_700",
    "v_component_of_wind_600",
    "v_component_of_wind_500",
    "v_component_of_wind_250",
    "v_component_of_wind_50",
    "temperature_925",
    "temperature_850",
    "temperature_700",
    "temperature_600",
    "temperature_500",
    "temperature_250",
    "temperature_50",
    "specific_humidity_925",
    "specific_humidity_850",
    "specific_humidity_700",
    "specific_humidity_600",
    "specific_humidity_500",
    "specific_humidity_250",
    "specific_humidity_50",
    "geopotential_925",
    "geopotential_850",
    "geopotential_700",
    "geopotential_600",
    "geopotential_500",
    "geopotential_250",
    "geopotential_50",
    "2m_temperature",
    "10m_u_component_of_wind",
    "10m_v_component_of_wind",
]

@click.command()
@click.option("--anal_dir", type=click.Path(exists=True))
@click.option("--gues_dir", type=click.Path(exists=False))
@click.option("--member_id", type=str, default="000000")
@click.option("--date_time", type=str, default="2006010100")
@click.option("--lead_time", type=int, default=6)
@click.option("--model_dir", type=click.Path(exists=True), default="models")
def main(
    anal_dir,
    gues_dir,
    member_id,
    date_time,
    lead_time,
    model_dir
):
    net = ClimaX(default_vars=DEFAULT_VARS)
    trained_path = os.path.join(model_dir, f"climax_lt_{lead_time}.ckpt")
    checkpoint = torch.load(trained_path, map_location=torch.device("cpu"))
    checkpoint_model = checkpoint["state_dict"]
    interpolate_pos_embed(net, checkpoint_model, new_size=(32, 64))
    state_dict = net.state_dict()

    for k in list(checkpoint_model.keys()):
        checkpoint_model[k.replace("net.", "")] = checkpoint_model[k]
        del checkpoint_model[k]
    for k in list(checkpoint_model.keys()):
        if "channel" in k:
            checkpoint_model[k.replace("channel", "var")] = checkpoint_model[k]
            del checkpoint_model[k]
    for k in list(checkpoint_model.keys()):
        if k not in state_dict.keys() or checkpoint_model[k].shape != state_dict[k].shape:
            del checkpoint_model[k]

    net.load_state_dict(checkpoint_model, strict=False)

    filename = os.path.join(anal_dir, member_id, f"{date_time}.grd")
    x = np.fromfile(filename, dtype=">f4")
    x = x.reshape(1, len(DEFAULT_VARS)-2, 32, 64)
    x = x[:, :-1, :, :]
    constants = np.fromfile("constants.grd", dtype=">f4")
    constants = constants.reshape(1, 4, 32, 64)
    constants = constants[:, :-1, :, :]
    x = np.concatenate([x, constants], axis=1)

    mean_path = "normalize_mean.npz"
    std_path = "normalize_std.npz"
    normalize_mean = dict(np.load(mean_path))
    normalize_std = dict(np.load(std_path))
    normalize_mean = np.concatenate([normalize_mean[var] for var in DEFAULT_VARS])
    normalize_std = np.concatenate([normalize_std[var] for var in DEFAULT_VARS])
    normalize = transforms.Normalize(normalize_mean, normalize_std)
    x = normalize(torch.tensor(x, dtype=torch.float32))
    denomalize = transforms.Normalize(-normalize_mean / normalize_std, 1 / normalize_std)

    net.eval()
    lead_time = torch.tensor([lead_time], dtype=torch.float32)
    _, preds = net(x, x, lead_time, DEFAULT_VARS, OUTPUT_VARS, None, 0.0)

    save_dir = os.path.join(gues_dir, member_id)
    os.makedirs(save_dir, exist_ok=True)

    constants = np.fromfile("constants.grd", dtype=">f4")
    constants = constants.reshape(4, 32, 64)
    constants = constants[:-1, :, :]
    constants = constants.byteswap().newbyteorder()
    constants = torch.tensor(constants, dtype=torch.float32)
    add_constants = torch.zeros(1, 3, 32, 64)
    preds = torch.cat([preds, add_constants], dim=1)
    preds = denomalize(preds)
    preds = preds[0, :len(OUTPUT_VARS), :, :]
    preds_con = torch.cat([preds, constants], dim=0)
    preds_con = preds_con.squeeze(0).detach().numpy()
    preds_dict = dict()
    for i in range(len(DEFAULT_VARS)):
        preds_dict[DEFAULT_VARS[i]] = preds_con[i, :, :]

    surface_pressure = np.zeros((1, 32, 64))
    g = 9.81
    M = 0.0289644
    R = 8.314
    levels = [925, 850, 700, 600, 500, 250, 50]

    for i in range(64):
        for j in range(32):
            for k in range(7):
                gph = preds_dict[f'geopotential_{levels[k]}'][j, i] / g
                if gph > preds_dict["orography"][j, i]:
                    surface_pressure[0, j, i] = levels[k]*100*np.exp(- g * M * (preds_dict["orography"][j, i] - gph) / (R * preds_dict[f"temperature_{levels[k]}"][j, i]))
                    break
    
    preds = preds.squeeze(0).detach().numpy()
    preds = np.concatenate([preds[:len(OUTPUT_VARS), :, :], surface_pressure], axis=0)
    # print(surface_pressure)
    # print(preds[-1, :, :])
    pred_date_time = datetime.strptime(date_time, "%Y%m%d%H") + timedelta(hours=lead_time.numpy().item())
    date_time = pred_date_time.strftime("%Y%m%d%H")
    preds.astype(np.float32).byteswap().newbyteorder().tofile(os.path.join(save_dir, f"{date_time}.grd"))

if __name__ == "__main__":
    main()
