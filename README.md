# Cardinal-and-Binary-Paired-Comparison-Models-Ranking-and-Forecasting
Cardinal and Binary Paired Comparison Models: Ranking and Forecasting for IPL Dataset
# Cardinal and Binary Paired Comparison Models: Ranking and Forecasting for IPL Dataset

This repository contains a modular pipeline of 8 Jupyter/Google Colab notebooks implementing **Cardinal** (e.g., Least Squares) and **Binary** paired comparison models for team ranking, subset selection, sensitivity analysis, and walk-forward match forecasting on the Indian Premier League (IPL) dataset.

---

## Repository Structure

Run the notebooks in numerical order, as later notebooks depend on functions, processed data, and variables loaded from earlier steps:

1. **`1_UnderstandingLeastSquareForCardinalComparison.ipynb`** — Mathematical foundations and Least Squares implementation for cardinal paired comparisons.
2. **`2_DataProcessing.ipynb`** — Cleaning, preprocessing, and formatting raw IPL match data into paired comparison matrices.
3. **`3_DummyImplementation.ipynb`** — Baseline and dummy model implementations to verify pipeline behavior.
4. **`4_RankingWithModels.ipynb`** — Fitting cardinal and binary paired comparison models to generate team rankings.
5. **`5_SubsetSelection.ipynb`** — Evaluating model stability and rankings across selected match/season subsets.
6. **`6_ComparingRanks.ipynb`** — Comparing rank correlations and discrepancies across different paired comparison methods.
7. **`7_SensitivityAnalysis.ipynb`** — Testing ranking sensitivity using bootstrapping.
8. **`8_WalkForwardPrediction.ipynb`** — Out-of-sample walk-forward forecasting and accuracy evaluation for upcoming IPL matches.

---

## How to Run the Notebooks

### Run in Google Colab (Recommended)
Because the notebooks chain together using `%run` commands inside Google Drive, follow these steps to run them smoothly in Colab:

1. **Download or clone the repository** onto your computer, then upload all 8 `.ipynb` files into a single folder inside your Google Drive (for example, `content/drive/MyDrive/Colab Notebooks/<file-name>`).
2. **Open `1_UnderstandingLeastSquareForCardinalComparison.ipynb`** in Google Colab by right-clicking the file in Google Drive and selecting **Open with > Google Colaboratory**.
3. **Mount your Google Drive** when prompted in the first cell so Colab can access the folder path (`/content/drive/MyDrive/Colab Notebooks`).
4. **Run notebooks in order (`1` through `8`).** Whenever a notebook executes `%run` on an earlier notebook, ensure the folder path matches where you saved the files in your Google Drive.

*(Tip: You can also open any notebook directly from GitHub in Colab by visiting `https://google.com` and uploading the remaining `.ipynb` files to your Colab session).*


## License
This project is licensed under the **MIT License** — see the [LICENSE](./LICENSE) file for details.
