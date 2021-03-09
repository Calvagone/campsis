[PK]
K=THETA_1*exp(ETA_1)
V=THETA_2*exp(ETA_2)
K12=THETA_3*exp(ETA_3)
K21=THETA_4*exp(ETA_4)
S1=V

[DES]
d/dt(A_CENTRAL)=K21*A_PERIPHERAL + (-K - K12)*A_CENTRAL
d/dt(A_PERIPHERAL)=K12*A_CENTRAL - K21*A_PERIPHERAL
d/dt(A_OUTPUT)=K*A_CENTRAL
F=A_CENTRAL/S1

[ERROR]
CP=F
OBS_CP=CP*(EPS_1 + 1)
Y=OBS_CP
