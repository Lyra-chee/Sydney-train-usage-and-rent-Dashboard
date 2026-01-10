import pandas as pd
import re

entry_paths = ["/Users/admin/Desktop/UNSW/5002/datasets/entry_exit.csv", "/Users/admin/Desktop/UNSW/5002/datasets/monthly_usage_pattern_train_data-june-2024.csv"]
syd_path   = "/Users/admin/Desktop/UNSW/5002/datasets/sydney_stations.csv"
station_type_accumulator = []


def normalize_name(name: str) -> str:
    if pd.isna(name):
        return ""
    # collapse multiple spaces, lowercase
    s = re.sub(r"\s+", " ", name).strip().lower()
    # drop trailing "railway station" or "station"
    s = re.sub(r"\s*(railway\s+)?station$", "", s).strip()
    return s

def normalize_station_label(name: str) -> str:
    if pd.isna(name):
        return name
    # collapse multiple spaces everywhere
    s = re.sub(r"\s+", " ", str(name)).strip()
    # unify any trailing variants to a single " Station" with one space before it
    core = re.sub(r"\s*(railway\s+)?station$", "", s, flags=re.I).strip()
    return f"{core} Station"

def normalize_trip(trip):
    if trip == "Less than 50":
        return 25
    return int(trip)

# for monthly_usage_pattern_train_data-june-2024.csv
def month_year_split_1(date):
    if pd.isna(date):
        return None, None
    month = date.split("-")[1]
    year = date.split("-")[0]
    return int(month), int(year)


def month_year_split_2(date):
    if pd.isna(date):
        return None, None
    tmp = date.split(" ")[0]
    month,year = tmp.split("-")[1],tmp.split("-")[0]
    return int(month), int(year)

def merge_data(entry_path, syd_path):
    df1 = pd.read_csv(entry_path)
    df2 = pd.read_csv(syd_path)
    df3 = pd.concat([df1, df2], ignore_index=True)
    df3.sort_values(by=["Station", "Year", "Month"], inplace=True)
    output_path = "/Users/admin/Desktop/UNSW/5002/datasets/entry_exit_sydney_merged.csv"
    df3.to_csv(output_path, index=False)
    



for entry_path in entry_paths:  
    df = pd.read_csv(entry_path)
    syd = pd.read_csv(syd_path)
    df.columns = [c.strip() for c in df.columns]
    df["Trip"] = df["Trip"].apply(normalize_trip)
    # for monthly_usage_pattern_train_data-june-2024.csv
    if "monthly_usage_pattern_train_data-june-2024.csv" in entry_path:
        split_vals = df["MonthYear"].apply(month_year_split_1).tolist()
        df[["Month", "Year"]] = pd.DataFrame(split_vals, index=df.index)
        output_path = "/Users/admin/Desktop/UNSW/5002/datasets/entry_exit_sydney_before_JUN_2024.csv"
    else:
        split_vals = df["MonthYear"].apply(month_year_split_2).tolist()
        output_path = "/Users/admin/Desktop/UNSW/5002/datasets/entry_exit_sydney_from_OCT_2024.csv"


    df[["Month", "Year"]] = pd.DataFrame(split_vals, index=df.index)
    df["station_key"]  = df["Station"].apply(normalize_name)
    syd_col = "Station" if "Station" in syd.columns else syd.columns[0]
    syd["station_key"] = syd[syd_col].apply(normalize_name)

    df_sydney = df[df["station_key"].isin(set(syd["station_key"]))].drop(columns=["station_key","MonthYear"])
    # ensure exactly one space before "Station" in output labels
    if "Station" in df_sydney.columns:
        df_sydney["Station"] = df_sydney["Station"].apply(normalize_station_label)
        station_type_accumulator.append(df_sydney[["Station","Station_Type"]].drop_duplicates())
        df_sydney=df_sydney.drop(columns=["Station_Type"])
    # collect Station -> Station_Type mapping and remove redundancy from output
    df_sydney.to_csv(output_path, index=False)

if station_type_accumulator:
    station_types = pd.concat(station_type_accumulator, ignore_index=True).drop_duplicates().sort_values(["Station"]) 
    station_types.to_csv("/Users/admin/Desktop/UNSW/5002/datasets/station_types.csv", index=False)

merge_data("/Users/admin/Desktop/UNSW/5002/datasets/entry_exit_sydney_before_JUN_2024.csv", "/Users/admin/Desktop/UNSW/5002/datasets/entry_exit_sydney_from_OCT_2024.csv")
