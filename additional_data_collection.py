# =============================================================================
# Additional Market Data Collection
# Project: Learning When Factors Work — Regime-Based Factor Rotation
# =============================================================================
# Output: additional_data.csv
#   VIX — monthly average of daily VIX closes (Yahoo Finance, from 1990)
# =============================================================================

import pandas as pd
import yfinance as yf
from datetime import datetime

END = datetime.today().strftime("%Y-%m-%d")


def fetch_vix() -> pd.DataFrame:
    print("Fetching VIX from Yahoo Finance...")
    raw = yf.download("^VIX", start="1990-01-01", end=END, progress=False)
    df = (
        raw["Close"].squeeze()
        .dropna()
        .resample("MS").mean()
        .rename("VIX")
        .reset_index()
        .rename(columns={"Date": "date"})
    )
    df["date"] = pd.to_datetime(df["date"]).dt.to_period("M").dt.to_timestamp()
    print(f"  VIX range: {df['date'].min().date()} to {df['date'].max().date()} ({len(df)} months)")
    return df


if __name__ == "__main__":
    vix_df = fetch_vix()
    vix_df["date"] = vix_df["date"].dt.date

    print(f"\nRows: {len(vix_df)}")
    print(vix_df.head())

    vix_df.to_csv("additional_data.csv", index=False)
    print("\nSaved: additional_data.csv")
