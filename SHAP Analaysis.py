#this code is used for the ananlysis and generation of SHAp 

import os
import shap
import xgboost as xgb
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt


###################For shap dotted plot ######################################
# Ensure output directory exists
output_dir = 'G:/python plot'
os.makedirs(output_dir, exist_ok=True)

# Load dataset
df = pd.read_csv('G:/flood training and its predictors.csv')

# Split features and target
X = df.drop(columns=['Training'])  
y = df['Training']

# Scale features
scaler = StandardScaler()
X_scaled = pd.DataFrame(scaler.fit_transform(X), columns=X.columns)

# Split data
X_train, X_test, y_train, y_test = train_test_split(X_scaled, y, test_size=0.2, random_state=42)

# Train XGBoost model
model = xgb.XGBClassifier(use_label_encoder=False, eval_metric='logloss')
model.fit(X_train, y_train)

# SHAP explainer
explainer = shap.TreeExplainer(model)
shap_values = explainer(X_test)

# SHAP summary plot with bold titles
plt.figure()
shap.summary_plot(shap_values, X_test, show=False)  # Disable immediate display

# Modify axis labels for boldness
plt.xlabel("SHAP Value (Impact on Model Output)", fontsize=14, fontweight='bold')
plt.ylabel("Feature Name", fontsize=14, fontweight='bold')

# Modify font properties for both x and y axis labels (including feature values on the right)
for label in plt.gca().get_xticklabels() + plt.gca().get_yticklabels():
    label.set_fontsize(14)  # Increase font size
    label.set_fontweight('bold')  # Make text bold

# Access the color bar and set bold font for feature value title
colorbar = plt.gca().collections[0].colorbar if plt.gca().collections else None

if colorbar:
    colorbar.ax.tick_params(labelsize=14, width=2)  # Increase font size for ticks
    colorbar.ax.set_yticklabels([str(int(tick)) for tick in colorbar.get_ticks()], fontsize=14, fontweight='bold')  # Set tick labels to bold

    # Set the title of the color bar (Feature Value) to be bold
    colorbar.set_label("Feature Value", fontsize=16, fontweight='bold')

# Save the summary plot
summary_plot_path = os.path.join(output_dir, "summary_plot.png")
plt.savefig(summary_plot_path, bbox_inches='tight', dpi=300)
plt.close()

# SHAP force plot (for the first sample)
plt.figure()
shap.plots.force(shap_values[0])  # Works in JupyterLab
plt.close()

print(f"Plots saved at:\n- {summary_plot_path}")

###############################For SHAP BarGraph########################################
import shap
import xgboost as xgb
import pandas as pd
import numpy as np
import os
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker

# Define output directory
output_dir = "G:/python plot"
os.makedirs(output_dir, exist_ok=True)

# Load dataset
df = pd.read_csv('G:/flood training and its predictors.csv')

# Split dataset into features (X) and target (y)
X = df.drop(columns=['Training'])  # Independent variables (features)
y = df['Training']  # Dependent variable (target)

# Scale the features
scaler = StandardScaler()
X_scaled = pd.DataFrame(scaler.fit_transform(X), columns=X.columns)

# Split data into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(X_scaled, y, test_size=0.2, random_state=42)

# Train XGBoost model
model = xgb.XGBClassifier()
model.fit(X_train, y_train)

# Create SHAP explainer
explainer = shap.Explainer(model, X_train)
shap_values = explainer(X_test)

# Convert SHAP values to a dataframe
shap_df = pd.DataFrame(shap_values.values, columns=X_test.columns)

# Compute mean absolute SHAP values
shap_mean = shap_df.abs().mean().sort_values(ascending=True)

# Generate bar plot
plt.figure(figsize=(12, 8))
bars = plt.barh(shap_mean.index, shap_mean.values, color='blue')
plt.xlabel("Mean Absolute SHAP Value", fontsize=14, fontweight='bold')
plt.ylabel("Feature", fontsize=14, fontweight='bold')
plt.title("Feature Importance Based on SHAP Values", fontsize=16, fontweight='bold')
plt.xticks(fontsize=12, fontweight='bold')
plt.yticks(fontsize=12, fontweight='bold')

# Add value labels to bars without overlap
for bar in bars:
    plt.text(bar.get_width() + 0.01, bar.get_y() + bar.get_height()/2, f'{bar.get_width():.2f}',
             va='center', fontsize=12, fontweight='bold', color='black')

plt.tight_layout()

# Ensure unique filename
existing_files = [f for f in os.listdir(output_dir) if f.startswith("SHAP") and f.endswith(".png")]
numbers = [int(f[4:-4]) for f in existing_files if f[4:-4].isdigit()]
next_number = max(numbers) + 1 if numbers else 1
filename = os.path.join(output_dir, f"SHAP{next_number}.png")

# Save and show the figure
plt.savefig(filename, dpi=300)
plt.show()

print(f"SHAP bar graph has been saved as {filename}.")

