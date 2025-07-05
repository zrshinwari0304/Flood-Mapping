import os
import numpy as np
import rasterio
import matplotlib.pyplot as plt
from scipy.stats import pearsonr

# Set directory and files
data_dir = "G:/ flood models"
file_names = {
    "2020": "flood2020.tif",
    "2040": "flood2040.tif",
    "2060": "flood2060.tif",
    "2080": "flood2080.tif",
    "2100": "flood2100.tif"
}

# Function to read and flatten raster
def read_raster(file_path):
    with rasterio.open(file_path) as src:
        data = src.read(1).astype(np.float32)
        data[data == src.nodata] = np.nan
        return data.flatten()

# Read reference data (2020)
ref_data = read_raster(os.path.join(data_dir, file_names["2020"]))

# Store metrics
std_devs = []
correlations = []
rmses = []
labels = []

for year in ["2040", "2060", "2080", "2100"]:
    model_data = read_raster(os.path.join(data_dir, file_names[year]))
    mask = ~np.isnan(ref_data) & ~np.isnan(model_data)
    ref_clean = ref_data[mask]
    model_clean = model_data[mask]

    std_model = np.std(model_clean)
    std_ref = np.std(ref_clean)
    corr, _ = pearsonr(ref_clean, model_clean)
    rmse = np.sqrt(np.mean((model_clean - ref_clean) ** 2))

    std_devs.append(std_model / std_ref)  # Normalized
    correlations.append(corr)
    rmses.append(rmse / std_ref)          # Normalized
    labels.append(year)

# --- Taylor Diagram with zero at bottom-left (rotated 90°) ---
fig = plt.figure(figsize=(10, 8))
ax = fig.add_subplot(111, polar=True)

ref_std = 1.0
max_std = 1.6
theta = np.linspace(0, np.pi / 2, 300)

# RMSE arcs
rmse_levels = [0.2, 0.4, 0.6, 0.8, 1.0]
rmse_handles = []
for rmse in rmse_levels:
    std_range = np.linspace(0, max_std, 300)
    r = np.sqrt(ref_std**2 + std_range**2 - 2 * ref_std * std_range * np.cos(theta))
    handle, = ax.plot(np.pi / 2 + theta, r, '--', color='gray', linewidth=0.8)
    rmse_handles.append(handle)
    ax.text(np.pi / 2 + np.pi / 4, rmse, f'$\mathbf{{{rmse:.1f}}}$', fontsize=12, fontweight='bold', color='gray', ha='center')

# Correlation lines and values (right arc)
cor_vals = np.linspace(0.1, 1.0, 10)
seen_angles = set()
for corr in cor_vals:
    angle = np.pi / 2 + np.arccos(corr)
    if round(angle, 3) not in seen_angles:
        seen_angles.add(round(angle, 3))
        ax.plot([angle, angle], [0, max_std], color='lightgray', linestyle='--')
        ax.text(angle, max_std + 0.05, f'$\mathbf{{{corr:.1f}}}$', fontsize=12, fontweight='bold', ha='center')

# Std deviation circles and values (left arc)
for r in np.arange(0.25, max_std + 0.1, 0.25):
    ax.plot(np.pi / 2 + theta, np.ones_like(theta) * r, color='lightgray', linestyle='--')
    ax.text(np.radians(93), r, f'$\mathbf{{{r:.2f}}}$', fontsize=12, fontweight='bold', va='center', ha='left')

# Reference point (2020)
ref_handle, = ax.plot(np.radians(90), ref_std, 'ko', markersize=10, label='2020 (Reference)')

# Future years
colors = ['red', 'blue', 'green', 'purple']
year_handles = []
for i, year in enumerate(labels):
    angle = np.pi / 2 + np.arccos(correlations[i])
    radius = std_devs[i]
    handle, = ax.plot(angle, radius, 'o', color=colors[i], markersize=10, label=year)
    year_handles.append(handle)

# Set orientation and layout
ax.set_theta_zero_location('S')  # South = bottom
ax.set_theta_direction(1)        # Counter-clockwise
ax.set_thetamin(90)
ax.set_thetamax(180)
ax.set_rmax(max_std + 0.15)
ax.set_rticks([0.5, 1.0, 1.5])
ax.set_rlabel_position(180)
ax.tick_params(labelsize=11)

# Title with full explanation
ax.set_title("Taylor Diagram (Zero at Bottom-Left)\n"
             "Normalized Standard Deviation (Left Arc), Correlation (Right Arc), Normalized RMSE Arcs\n"
             "Comparison: 2020 (Reference) vs Future Scenarios: 2040, 2060, 2080, 2100",
             fontsize=14, fontweight='bold', y=1.15)

# Legend with bold font and larger size
legend_handles = [ref_handle] + year_handles + rmse_handles
legend_labels = ["2020 (Reference)"] + labels + [f'RMSE {lvl:.1f}' for lvl in rmse_levels]
legend = ax.legend(legend_handles, legend_labels, loc='upper right', bbox_to_anchor=(1.35, 1.05), fontsize=11)
for text in legend.get_texts():
    text.set_fontweight('bold')

# Save the plot in high resolution (600 dpi) and show
plt.tight_layout()

# Save the plot
plt.savefig("G:/taylor_diagram.png", dpi=600, bbox_inches='tight')

# Show the plot
plt.show()
