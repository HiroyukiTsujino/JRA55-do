dset ^/work115/htsujino/SURF_FLUX/forcing/jra_monthly_f1/sverdrup.%y4%m2
undef -9.99E+33
title fcst_surf
options template big_endian
ydef 320 levels   -90.152   -89.443   -88.885   -88.324   -87.763   -87.202   -86.640   -86.079
   -85.518   -84.956   -84.394   -83.833   -83.271   -82.710   -82.148   -81.587
   -81.025   -80.463   -79.902   -79.340   -78.779   -78.217   -77.655   -77.094
   -76.532   -75.971   -75.409   -74.847   -74.286   -73.724   -73.162   -72.601
   -72.039   -71.478   -70.916   -70.354   -69.793   -69.231   -68.670   -68.108
   -67.546   -66.985   -66.423   -65.861   -65.300   -64.738   -64.177   -63.615
   -63.053   -62.492   -61.930   -61.369   -60.807   -60.245   -59.684   -59.122
   -58.560   -57.999   -57.437   -56.876   -56.314   -55.752   -55.191   -54.629
   -54.067   -53.506   -52.944   -52.383   -51.821   -51.259   -50.698   -50.136
   -49.575   -49.013   -48.451   -47.890   -47.328   -46.766   -46.205   -45.643
   -45.082   -44.520   -43.958   -43.397   -42.835   -42.273   -41.712   -41.150
   -40.589   -40.027   -39.465   -38.904   -38.342   -37.780   -37.219   -36.657
   -36.096   -35.534   -34.972   -34.411   -33.849   -33.287   -32.726   -32.164
   -31.603   -31.041   -30.479   -29.918   -29.356   -28.795   -28.233   -27.671
   -27.110   -26.548   -25.986   -25.425   -24.863   -24.302   -23.740   -23.178
   -22.617   -22.055   -21.493   -20.932   -20.370   -19.809   -19.247   -18.685
   -18.124   -17.562   -17.000   -16.439   -15.877   -15.316   -14.754   -14.192
   -13.631   -13.069   -12.507   -11.946   -11.384   -10.823   -10.261    -9.699
    -9.138    -8.576    -8.015    -7.453    -6.891    -6.330    -5.768    -5.206
    -4.645    -4.083    -3.522    -2.960    -2.398    -1.837    -1.275    -0.713
    -0.152     0.410     0.971     1.533     2.095     2.656     3.218     3.780
     4.341     4.903     5.464     6.026     6.588     7.149     7.711     8.273
     8.834     9.396     9.957    10.519    11.081    11.642    12.204    12.765
    13.327    13.889    14.450    15.012    15.574    16.135    16.697    17.258
    17.820    18.382    18.943    19.505    20.067    20.628    21.190    21.751
    22.313    22.875    23.436    23.998    24.560    25.121    25.683    26.244
    26.806    27.368    27.929    28.491    29.053    29.614    30.176    30.737
    31.299    31.861    32.422    32.984    33.545    34.107    34.669    35.230
    35.792    36.354    36.915    37.477    38.038    38.600    39.162    39.723
    40.285    40.847    41.408    41.970    42.531    43.093    43.655    44.216
    44.778    45.340    45.901    46.463    47.024    47.586    48.148    48.709
    49.271    49.833    50.394    50.956    51.517    52.079    52.641    53.202
    53.764    54.325    54.887    55.449    56.010    56.572    57.134    57.695
    58.257    58.818    59.380    59.942    60.503    61.065    61.627    62.188
    62.750    63.311    63.873    64.435    64.996    65.558    66.119    66.681
    67.243    67.804    68.366    68.928    69.489    70.051    70.612    71.174
    71.736    72.297    72.859    73.420    73.982    74.544    75.105    75.667
    76.229    76.790    77.352    77.913    78.475    79.037    79.598    80.160
    80.721    81.283    81.845    82.406    82.968    83.529    84.091    84.652
    85.214    85.776    86.337    86.898    87.460    88.021    88.581    89.140
xdef 640 linear -0.28125 0.562500
tdef 29280 linear jan1999 1mo
zdef 1 linear 1 1
vars 2
sv    0 33,105,10 ** 10 m above ground u-component of wind [m/s]
curl  0 33,105,10 ** 10 m above ground u-component of wind [m/s]
ENDVARS
