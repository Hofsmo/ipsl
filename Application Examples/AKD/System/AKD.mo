within AKD.System;
model AKD
  constant Real pi=Modelica.Constants.pi;
  Components.Gen_no_contr gen58553(
    V_b=130,
    M_b=120,
    Tpd0=7.34,
    Tppd0=0.05,
    Tppq0=0.25,
    H=2.31,
    D=0,
    Xd=1.2020,
    Xq=0.658,
    Xpd=0.3060,
    Xppd=0.194,
    Xl=0.113,
    S10=0.08,
    S12=0.245,
    R_a=0.004,
    V_0=PF_data.voltages.V58553,
    angle_0=PF_data.voltages.A58553,
    P_0=PF_data.powers.P58553,
    Q_0=PF_data.powers.Q58553) annotation (Placement(transformation(extent={{-94,-2},{-70,20}})));
  Components.Gen_no_contr gen58583(
    V_b=130,
    M_b=120,
    Tpd0=7.34,
    Tppd0=0.05,
    Tppq0=0.25,
    H=2.31,
    D=0,
    Xd=1.2020,
    Xq=0.6580,
    Xpd=0.3060,
    Xppd=0.1940,
    Xl=0.113,
    S10=0.08,
    S12=0.245,
    R_a=0.004,
    V_0=PF_data.voltages.V58583,
    angle_0=PF_data.voltages.A58583,
    P_0=PF_data.powers.P58583,
    Q_0=PF_data.powers.Q58583) annotation (Placement(transformation(extent={{-34,33},{-10,55}})));
  Components.Gen_gov_exc_stab gen58567(
    V_b=14.7,
    M_b=120,
    Tpd0=7.9,
    Tppd0=0.05,
    Tppq0=0.25,
    H=2.93,
    D=0,
    Xd=1.13,
    Xq=0.632,
    Xpd=0.31,
    Xppd=0.194,
    Xl=0.123,
    S10=0.0607,
    S12=0.232,
    R_a=0.004,
    R=0.08,
    r=0.4,
    T_r=8,
    T_f=0.05,
    T_g=0.2,
    VELM=0.2,
    G_MAX=1,
    G_MIN=0,
    T_w=1.2,
    A_t=1.1,
    D_turb=0.3,
    q_NL=0.1,
    T_R=0.06,
    K_A=150,
    T_A=0,
    V_RMAX=4,
    V_RMIN=-4,
    K_E=1,
    T_E=0.04,
    K_F=0.011,
    T_F1=0.4,
    T_F2=0.7,
    E_1=6.5,
    S_EE_1=0.054,
    E_2=8,
    S_EE_2=0.2020,
    A_1=3,
    A_2=0,
    A_3=0,
    A_4=0,
    A_5=0,
    A_6=0,
    T_1=10000,
    T_2=3,
    T_3=1,
    T_4=3,
    T_5=0.001,
    T_6=3,
    K_S=-13.5,
    L_SMAX=0.03,
    L_SMIN=-0.03,
    V_CU=0,
    V_CL=0,
    V_0=PF_data.voltages.V58567,
    angle_0=PF_data.voltages.A58567,
    P_0=PF_data.powers.P58567,
    Q_0=PF_data.powers.Q58567) annotation (Placement(transformation(extent={{-102,-46},{-78,-24}})));
  Components.Gen_no_contr gen58573(
    V_b=130,
    M_b=120,
    Tpd0=7.34,
    Tppd0=0.05,
    Tppq0=0.25,
    H=2.31,
    D=0,
    Xd=1.2020,
    Xq=0.658,
    Xpd=0.306,
    Xppd=0.194,
    Xl=0.113,
    S10=0.08,
    S12=0.245,
    R_a=0.004,
    V_0=PF_data.voltages.V58573,
    angle_0=PF_data.voltages.A58573,
    P_0=PF_data.powers.P58573,
    Q_0=PF_data.powers.Q58573) annotation (Placement(transformation(extent={{82,4},{60,26}})));
  iPSL.Electrical.Branches.PwLine_Bis line_58553_58563(
    R=0.19224,
    X=0.78070,
    G=0,
    B=0.00257*0.5) annotation (Placement(transformation(extent={{-40,-18},{-20,2}})));
  iPSL.Electrical.Branches.PwLine_Bis line_58563_58573(
    R=0.37478,
    X=1.52958,
    G=0,
    B=0.00408*0.5) annotation (Placement(transformation(extent={{20,-18},{40,2}})));
  iPSL.Electrical.Branches.PwLine_Bis line_58553_58583(
    R=0.22669,
    X=1.30550,
    B=0.00526*0.5,
    G=0) annotation (Placement(transformation(extent={{-40,6},{-20,26}})));
  iPSL.Electrical.Branches.PwLine_Bis line_58573_58583(
    R=0.25195,
    X=1.30183,
    G=0,
    B=0.00272*0.5) annotation (Placement(transformation(extent={{20,6},{40,26}})));
  Components.Gen_no_contr gen58653(
    V_b=130,
    M_b=120,
    Tpd0=7.34,
    Tppd0=0.05,
    Tppq0=0.25,
    H=2.31,
    D=0,
    Xd=1.2020,
    Xq=0.658,
    Xpd=0.306,
    Xppd=0.194,
    Xl=0.113,
    S10=0.08,
    S12=0.245,
    R_a=0.004,
    V_0=PF_data.voltages.V58653,
    angle_0=PF_data.voltages.A58653,
    P_0=PF_data.powers.P58653,
    Q_0=PF_data.powers.Q58653) annotation (Placement(transformation(extent={{120,-11},{98,11}})));
  iPSL.Electrical.Branches.PwLine_Bis line_58573_58653(
    G=0,
    R=0.49047,
    X=2.01225,
    B=0.00404*0.5) annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=0,
        origin={68,-1})));
  iPSL.Electrical.Buses.BusExt bus_58583(
    no=2,
    nu=1,
    V_0=PF_data.voltages.V58583,
    angle_0=PF_data.voltages.A58583) annotation (Placement(transformation(extent={{-2,30},{0,50}})));
  iPSL.Electrical.Buses.BusExt bus_58553(
    no=1,
    nu=2,
    V_0=PF_data.voltages.V58553,
    angle_0=PF_data.voltages.A58553) annotation (Placement(transformation(extent={{-62,-2},{-60,18}})));
  iPSL.Electrical.Buses.BusExt bus_58563(
    nu=3,
    no=2,
    V_0=PF_data.voltages.V58563,
    angle_0=PF_data.voltages.A58563) annotation (Placement(transformation(extent={{-2,-50},{0,4}})));
  iPSL.Electrical.Buses.BusExt bus_58567(
    no=1,
    nu=1,
    V_0=PF_data.voltages.V58567,
    angle_0=PF_data.voltages.A58567) annotation (Placement(transformation(extent={{-70,-45},{-68,-25}})));
  iPSL.Electrical.Buses.BusExt bus_58566(
    no=1,
    nu=1,
    V_0=PF_data.voltages.V58566,
    angle_0=PF_data.voltages.A58566) annotation (Placement(transformation(extent={{69,-45},{71,-25}})));
  iPSL.Electrical.Buses.BusExt bus_58573(
    no=2,
    nu=2,
    V_0=PF_data.voltages.V58573,
    angle_0=PF_data.voltages.A58573) annotation (Placement(transformation(extent={{50,-12},{52,24}})));
  iPSL.Electrical.Buses.BusExt bus_58653(
    no=1,
    nu=1,
    V_0=PF_data.voltages.V58653,
    angle_0=PF_data.voltages.A58653) annotation (Placement(transformation(extent={{88,-12},{90,10}})));

  iPSL.Electrical.Banks.PwShuntB shunt_58553_58563(B=-0.0005) annotation (Placement(transformation(extent={{-46,-22},{-38,-14}})));
  iPSL.Electrical.Banks.PwShuntB shunt_58563_58553(B=0.0005) annotation (Placement(transformation(extent={{-22,-22},{-14,-14}})));
  iPSL.Electrical.Banks.PwShuntB shunt_58563_58573(B=0.0005) annotation (Placement(transformation(extent={{16,-22},{24,-14}})));
  iPSL.Electrical.Banks.PwShuntB shunt_58573_58563(B=-0.0005) annotation (Placement(transformation(extent={{38,-22},{46,-14}})));
  Components.equiv_trafo trafo_58563_58567(
    R=0.027500,
    X=0.989170,
    G=0,
    B=0,
    t1=1,
    t2=1,
    PrimaryOnFromSide=true) annotation (Placement(transformation(extent={{-53,-49},{-21,-21}})));
  Components.equiv_trafo trafo_58563_58566(
    R=0.065,
    X=2.113330,
    G=0,
    B=0,
    t1=1,
    t2=1,
    PrimaryOnFromSide=true) annotation (Placement(transformation(extent={{56,-49},{24,-21}})));
  Data.PF_data PF_data annotation (Placement(transformation(extent={{-80,60},{-60,80}})));
  inner iPSL.Electrical.SystemBase SysData(S_b=1000) annotation (Placement(transformation(extent={{72,60},{110,80}})));
  iPSL.Electrical.Loads.PSSE.Load Load_58563(
    V_b=130,
    P_0=8.29,
    Q_0=0,
    V_0=PF_data.voltages.V58563,
    angle_0=PF_data.voltages.A58563,
    characteristic=2) annotation (Placement(transformation(extent={{13,-64},{27,-52}})));
  Components.Gen_gov_exc_stab gen58566(
    V_b=9.5,
    M_b=60,
    Tpd0=7.34,
    Tppd0=0.05,
    Tppq0=0.25,
    H=2.31,
    D=0,
    Xd=1.202,
    Xq=0.658,
    Xpd=0.306,
    Xppd=0.187,
    Xl=0.113,
    S10=0.08,
    S12=0.245,
    R_a=0.005,
    R=0.08,
    r=0.4,
    T_r=8,
    T_f=0.05,
    T_g=0.2,
    VELM=0.2,
    G_MAX=1,
    G_MIN=0,
    T_w=1.2,
    A_t=1.1,
    D_turb=0.3,
    q_NL=0.1,
    T_R=0.06,
    K_A=150,
    T_A=0,
    V_RMAX=4,
    V_RMIN=-4,
    K_E=1,
    T_E=0.04,
    K_F=0.011,
    T_F1=0.4,
    T_F2=0.7,
    E_1=6.5,
    S_EE_1=0.054,
    E_2=8,
    S_EE_2=0.202,
    T_1=10000,
    T_2=3,
    T_3=1,
    T_4=3,
    T_5=0.001,
    T_6=3,
    K_S=-13.5,
    L_SMAX=0.03,
    L_SMIN=-0.03,
    V_CU=0,
    V_CL=0,
    V_0=PF_data.voltages.V58566,
    angle_0=PF_data.voltages.A58566,
    P_0=PF_data.powers.P58566,
    Q_0=PF_data.powers.Q58566) annotation (Placement(transformation(extent={{112,-46},{90,-24}})));
equation
  connect(line_58553_58583.n, bus_58583.o[1]) annotation (Line(
      points={{-23,16},{-6,16},{-6,37},{-2,37}},
      color={0,0,255},
      smooth=Smooth.None));
  connect(gen58583.pwPin, bus_58583.o[2]) annotation (Line(
      points={{-13.6,44},{-6.84,44},{-6.84,43},{-2,43}},
      color={0,0,255},
      smooth=Smooth.None));
  connect(bus_58583.u[1], line_58573_58583.p) annotation (Line(
      points={{0,40},{5,40},{5,16},{23,16}},
      color={0,0,255},
      smooth=Smooth.None));
  connect(gen58553.pwPin, bus_58553.o[1]) annotation (Line(
      points={{-73.6,9},{-66.84,9},{-66.84,8},{-62,8}},
      color={0,0,255},
      smooth=Smooth.None));
  connect(bus_58553.u[1], line_58553_58563.p) annotation (Line(
      points={{-60,5},{-56,5},{-56,-8},{-37,-8}},
      color={0,0,255},
      smooth=Smooth.None));
  connect(line_58553_58583.p, bus_58553.u[2]) annotation (Line(
      points={{-37,16},{-56,16},{-56,11},{-60,11}},
      color={0,0,255},
      smooth=Smooth.None));
  connect(line_58563_58573.n, bus_58573.o[1]) annotation (Line(
      points={{37,-8},{44,-8},{44,0.6},{50,0.6}},
      color={0,0,255},
      smooth=Smooth.None));
  connect(line_58573_58583.n, bus_58573.o[2]) annotation (Line(
      points={{37,16},{44,16},{44,11.4},{50,11.4}},
      color={0,0,255},
      smooth=Smooth.None));
  connect(line_58573_58653.p, bus_58573.u[1]) annotation (Line(
      points={{61,-1},{56.5,-1},{56.5,0.6},{52,0.6}},
      color={0,0,255},
      smooth=Smooth.None));
  connect(gen58573.pwPin, bus_58573.u[2]) annotation (Line(
      points={{63.3,15},{56.77,15},{56.77,11.4},{52,11.4}},
      color={0,0,255},
      smooth=Smooth.None));
  connect(line_58573_58653.n, bus_58653.o[1]) annotation (Line(
      points={{75,-1},{88,-1}},
      color={0,0,255},
      smooth=Smooth.None));
  connect(bus_58653.u[1], gen58653.pwPin) annotation (Line(
      points={{90,-1},{94.77,-1},{94.77,0},{101.3,0}},
      color={0,0,255},
      smooth=Smooth.None));
  connect(shunt_58553_58563.p, line_58553_58563.p) annotation (Line(
      points={{-42.16,-14.96},{-42.16,-11.48},{-37,-11.48},{-37,-8}},
      color={0,0,255},
      smooth=Smooth.None));
  connect(shunt_58563_58553.p, line_58553_58563.n) annotation (Line(
      points={{-18.16,-14.96},{-18.16,-11.48},{-23,-11.48},{-23,-8}},
      color={0,0,255},
      smooth=Smooth.None));
  connect(shunt_58563_58573.p, line_58563_58573.p) annotation (Line(
      points={{19.84,-14.96},{19.84,-12.48},{23,-12.48},{23,-8}},
      color={0,0,255},
      smooth=Smooth.None));
  connect(shunt_58573_58563.p, line_58563_58573.n) annotation (Line(
      points={{41.84,-14.96},{41.84,-12.48},{37,-12.48},{37,-8}},
      color={0,0,255},
      smooth=Smooth.None));
  connect(trafo_58563_58567.pTo, bus_58567.u[1]) annotation (Line(
      points={{-47.56,-34.93},{-58.28,-34.93},{-58.28,-35},{-68,-35}},
      color={0,0,255},
      smooth=Smooth.None));
  connect(trafo_58563_58566.pTo, bus_58566.o[1]) annotation (Line(
      points={{50.56,-34.93},{59.28,-34.93},{59.28,-35},{69,-35}},
      color={0,0,255},
      smooth=Smooth.None));
  connect(gen58567.pwPin, bus_58567.o[1]) annotation (Line(points={{-81.6,-35},{-75.8,-35},{-70,-35}}, color={0,0,255}));
  connect(Load_58563.p, bus_58563.u[1]) annotation (Line(points={{20,-51.4},{20,-42},{6,-42},{6,-38},{6,-34},{6,-33.8},{0,-33.8}}, color={0,0,255}));
  connect(trafo_58563_58566.pFrom, bus_58563.u[2]) annotation (Line(points={{29.44,-34.93},{14.72,-34.93},{14.72,-23},{0,-23}}, color={0,0,255}));
  connect(line_58563_58573.p, bus_58563.u[3]) annotation (Line(points={{23,-8},{12,-8},{12,-12.2},{0,-12.2}}, color={0,0,255}));
  connect(trafo_58563_58567.pFrom, bus_58563.o[1]) annotation (Line(points={{-26.44,-34.93},{-14.22,-34.93},{-14.22,-31.1},{-2,-31.1}}, color={0,0,255}));
  connect(line_58553_58563.n, bus_58563.o[2]) annotation (Line(points={{-23,-8},{-12,-8},{-12,-14.9},{-2,-14.9}}, color={0,0,255}));
  connect(gen58566.pwPin, bus_58566.u[1]) annotation (Line(points={{93.3,-35},{82.65,-35},{71,-35}}, color={0,0,255}));
  annotation (
    Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{120,100}}), graphics={
        Text(
          extent={{-12,56},{8,50}},
          lineColor={0,128,0},
          textStyle={TextStyle.Bold},
          textString="bus 58583"),
        Text(
          extent={{-72,24},{-52,18}},
          lineColor={0,128,0},
          textStyle={TextStyle.Bold},
          textString="bus 58553"),
        Text(
          extent={{-79,-45},{-59,-51}},
          lineColor={0,128,0},
          textStyle={TextStyle.Bold},
          textString="bus 58567"),
        Text(
          extent={{60,-46},{80,-52}},
          lineColor={0,128,0},
          textStyle={TextStyle.Bold},
          textString="bus 58566"),
        Text(
          extent={{-11,-51},{9,-57}},
          lineColor={0,128,0},
          textStyle={TextStyle.Bold},
          textString="bus 58563"),
        Text(
          extent={{40,30},{60,24}},
          lineColor={0,128,0},
          textStyle={TextStyle.Bold},
          textString="bus 58573"),
        Text(
          extent={{80,16},{100,10}},
          lineColor={0,128,0},
          textStyle={TextStyle.Bold},
          textString="bus 58653")}),
    Icon(coordinateSystem(extent={{-100,-100},{120,100}})),
    Documentation(revisions="<html>
<!--DISCLAIMER-->
<p>Copyright 2015-2016 RTE (France), SmarTS Lab (Sweden), AIA (Spain) and DTU (Denmark)</p>
<ul>
<li>RTE: <a href=\"http://www.rte-france.com\">http://www.rte-france.com</a></li>
<li>SmarTS Lab, research group at KTH: <a href=\"https://www.kth.se/en\">https://www.kth.se/en</a></li>
<li>AIA: <a href=\"http://www.aia.es/en/energy\"> http://www.aia.es/en/energy</a></li>
<li>DTU: <a href=\"http://www.dtu.dk/english\"> http://www.dtu.dk/english</a></li>
</ul>
<p>The authors can be contacted by email: <a href=\"mailto:info@itesla-ipsl.org\">info@itesla-ipsl.org</a></p>

<p>This Source Code Form is subject to the terms of the Mozilla Public License, v. 2.0. </p>
<p>If a copy of the MPL was not distributed with this file, You can obtain one at <a href=\"http://mozilla.org/MPL/2.0/\"> http://mozilla.org/MPL/2.0</a>.</p>
</html>"));
end AKD;
