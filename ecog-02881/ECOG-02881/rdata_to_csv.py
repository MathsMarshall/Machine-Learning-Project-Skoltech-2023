import pandas as pd
import pyreadr


result = pyreadr.read_r("Appendix_6_Box_4_DATA_NorthAmerica_DougFir.rdata") # Read .rdata file
df = result['tr'] #convert R data to pandas DataFrame
df.to_csv("Box4_data.csv",index=False)
