within IEEE9;
model IEEE_9_Buses_Statcom
  import Modelica.Constants.pi;
  iPSL.Electrical.Branches.PSAT.TwoWindingTransformer twoWindingTransformer(
    kT=16.5/230,
    x=0.0576,
    r=0,
    V_b=16.5,
    Vn=16.5) annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=90,
        origin={0,-66})));
  iPSL.Electrical.Branches.PwLine line_6_4(
    R=0.017,
    X=0.092,
    G=0,
    B=0.079) annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=90,
        origin={86,-20})));
  iPSL.Electrical.Branches.PwLine line_4_5(
    G=0,
    R=0.01,
    X=0.085,
    B=0.088) annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=90,
        origin={-72,-20})));
  iPSL.Electrical.Loads.PSAT.VoltDependant lOADPQ(
    V_b=230,
    V_0=0.997968971576412,
    angle_0=-0.069547629203294*180/pi,
    P_0=1.25*SysData.S_b,
    Q_0=0.5*SysData.S_b) annotation (Placement(transformation(
        extent={{-7,-7},{7,7}},
        rotation=0,
        origin={-99,-9})));
  iPSL.Electrical.Loads.PSAT.VoltDependant PQ1(
    V_b=230,
    V_0=1.014662489636656,
    angle_0=-0.064300235023830*180/pi,
    P_0=0.9*SysData.S_b,
    Q_0=0.3*SysData.S_b) annotation (Placement(transformation(
        extent={{-7,-7},{7,7}},
        rotation=0,
        origin={109,-13})));
  iPSL.Electrical.Branches.PwLine line_9_6(
    G=0,
    R=0.039,
    X=0.170,
    B=0.179) annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=90,
        origin={86,58})));
  iPSL.Electrical.Branches.PwLine line_5_7(
    G=0,
    R=0.032,
    X=0.161,
    B=0.153) annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=90,
        origin={-72,54})));
  iPSL.Electrical.Branches.PwLine line_8_9(
    G=0,
    R=0.0119,
    X=0.1008,
    B=0.1045) annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=0,
        origin={40,90})));
  iPSL.Electrical.Branches.PSAT.TwoWindingTransformer twoWindingTransformer1(
    r=0,
    kT=13.8/230,
    x=0.0586,
    V_b=13.8,
    Vn=13.8) annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=180,
        origin={102,90})));
  iPSL.Electrical.Branches.PSAT.TwoWindingTransformer twoWindingTransformer2(
    r=0,
    kT=18/230,
    x=0.0625,
    Sn=100,
    V_b=18,
    Vn=18) annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=0,
        origin={-102,90})));
  iPSL.Electrical.Loads.PSAT.VoltDependant lOADPQ1(
    V_0=1.025,
    angle_0=0.011660880329004*180/pi,
    V_b=230,
    P_0=1*SysData.S_b,
    Q_0=0.35*SysData.S_b) annotation (Placement(transformation(
        extent={{-7,-7},{7,7}},
        rotation=0,
        origin={-9,65})));
  iPSL.Electrical.Controls.PSAT.FACTS.STATCOM.STATCOM sTATCOM3_1(
    Qg=0.128730182132440,
    Tr=0.1,
    Sn=100,
    fn=60,
    V_0=1.025,
    Vbus=230,
    Vn=230,
    Sb=SysData.S_b,
    angle_0=0.011660880329004*180/pi,
    i_Min=-0.8,
    Kr=100,
    i_Max=1.2) annotation (Placement(transformation(
        extent={{-6,-6},{6,6}},
        rotation=90,
        origin={12,66})));
  iPSL.Electrical.Buses.Bus B2 annotation (Placement(transformation(extent={{-130,80},{-110,100}})));
  iPSL.Electrical.Buses.Bus B7 annotation (Placement(transformation(extent={{-90,80},{-70,100}})));
  iPSL.Electrical.Buses.Bus B8(V_0=1.025) annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=0,
        origin={0,90})));
  iPSL.Electrical.Buses.Bus B9 annotation (Placement(transformation(extent={{70,80},{90,100}})));
  iPSL.Electrical.Buses.Bus B3 annotation (Placement(transformation(extent={{110,80},{130,100}})));
  iPSL.Electrical.Buses.Bus B6 annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=-90,
        origin={86,20})));
  iPSL.Electrical.Buses.Bus B5 annotation (Placement(transformation(
        extent={{-12,-12},{12,12}},
        rotation=90,
        origin={-72,20})));
  iPSL.Electrical.Buses.Bus B4 annotation (Placement(transformation(
        extent={{-12,-12},{12,12}},
        rotation=-90,
        origin={0,-46})));
  iPSL.Electrical.Buses.Bus B1 annotation (Placement(transformation(
        extent={{-12,-12},{12,12}},
        rotation=-90,
        origin={0,-86})));
  iPSL.Electrical.Branches.PwLine2Openings ine_7_8(
    R=0.0085,
    X=0.072,
    G=0,
    B=0.0745,
    t2=110,
    t1=30) annotation (Placement(transformation(extent={{-50,80},{-30,100}})));
  Generation_Groups.Gen1 gen1(
    V_b=18,
    V_0=1.025,
    height_1=0.05,
    tstart_1=2,
    angle_0=0.160490018910725*180/pi,
    P_0=1.63*SysData.S_b,
    Q_0=0.001552891584958*SysData.S_b,
    refdisturb_1=false) annotation (Placement(transformation(extent={{-160,80},{-140,100}})));
  Generation_Groups.Gen2 gen2(
    V_b=13.8,
    V_0=1.025,
    height_2=0.05,
    tstart_2=2,
    refdisturb_2=false,
    P_0=0.85*SysData.S_b,
    Q_0=-0.163501111031896*SysData.S_b,
    angle_0=0.080629575357894*180/pi) annotation (Placement(transformation(extent={{160,80},{140,100}})));
  Generation_Groups.Gen3 gen3(
    V_0=1.040000000000000,
    angle_0=0,
    height_3=0.05,
    tstart_3=2,
    refdisturb_3=false,
    V_b=16.5,
    Q_0=0.248141030193284*SysData.S_b,
    P_0=0.715870954698346*SysData.S_b) annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=90,
        origin={0,-106})));
  inner iPSL.Electrical.SystemBase SysData(S_b=100, fn=60) annotation (Placement(transformation(extent={{120,-120},{180,-80}})));
  iPSL.Electrical.Events.PwFault pwFault2(
    X=0.01,
    R=0.01,
    t1=3,
    t2=3.1) annotation (Placement(transformation(
        extent={{-7,-7},{7,7}},
        rotation=270,
        origin={59,67})));
equation

  connect(line_5_7.n, B7.p) annotation (Line(
      points={{-72,61},{-72,90},{-80,90}},
      color={0,0,255},
      smooth=Smooth.None));
  connect(twoWindingTransformer1.n, B9.p) annotation (Line(
      points={{91,90},{80,90}},
      color={0,0,255},
      smooth=Smooth.None));
  connect(line_9_6.n, B9.p) annotation (Line(
      points={{86,65},{86,90},{80,90}},
      color={0,0,255},
      smooth=Smooth.None));
  connect(twoWindingTransformer1.p, B3.p) annotation (Line(
      points={{113,90},{120,90}},
      color={0,0,255},
      smooth=Smooth.None));
  connect(line_6_4.n, B6.p) annotation (Line(
      points={{86,-13},{86,20}},
      color={0,0,255},
      smooth=Smooth.None));
  connect(B6.p, line_9_6.p) annotation (Line(
      points={{86,20},{86,51}},
      color={0,0,255},
      smooth=Smooth.None));
  connect(line_4_5.n, B5.p) annotation (Line(
      points={{-72,-13},{-72,20}},
      color={0,0,255},
      smooth=Smooth.None));
  connect(line_5_7.p, B5.p) annotation (Line(
      points={{-72,47},{-72,20}},
      color={0,0,255},
      smooth=Smooth.None));
  connect(twoWindingTransformer.p, B1.p) annotation (Line(
      points={{0,-77},{0,-86}},
      color={0,0,255},
      smooth=Smooth.None));

  connect(B7.p, ine_7_8.p) annotation (Line(points={{-80,90},{-47,90}}, color={0,0,255}));
  connect(ine_7_8.n, B8.p) annotation (Line(points={{-33,90},{-33,90},{0,90}}, color={0,0,255}));
  connect(B2.p, gen1.pwPin) annotation (Line(points={{-120,90},{-139,90}}, color={0,0,255}));
  connect(B2.p, twoWindingTransformer2.p) annotation (Line(points={{-120,90},{-110,90},{-113,90}}, color={0,0,255}));
  connect(twoWindingTransformer2.n, B7.p) annotation (Line(points={{-91,90},{-91,90},{-80,90}}, color={0,0,255}));
  connect(gen2.pwPin, B3.p) annotation (Line(points={{139,90},{129.5,90},{120,90}}, color={0,0,255}));
  connect(lOADPQ.p, B5.p) annotation (Line(points={{-99,-1.3},{-99,4},{-72,4},{-72,20}}, color={0,0,255}));
  connect(line_4_5.p, B4.p) annotation (Line(points={{-72,-27},{-72,-27},{-72,-42},{0,-42},{0,-46}}, color={0,0,255}));
  connect(line_6_4.p, B4.p) annotation (Line(points={{86,-27},{86,-42},{0,-42},{0,-46}}, color={0,0,255}));
  connect(PQ1.p, B6.p) annotation (Line(points={{109,-5.3},{109,0},{86,0},{86,20}}, color={0,0,255}));
  connect(lOADPQ1.p, B8.p) annotation (Line(points={{-9,72.7},{-9,90},{0,90}}, color={0,0,255}));
  connect(B4.p, twoWindingTransformer.n) annotation (Line(points={{0,-46},{0,-55}}, color={0,0,255}));
  connect(gen3.pwPin, B1.p) annotation (Line(points={{0,-95},{0,-86}}, color={0,0,255}));
  connect(line_8_9.n, B9.p) annotation (Line(points={{47,90},{63.5,90},{80,90}}, color={0,0,255}));
  connect(B8.p, line_8_9.p) annotation (Line(points={{0,90},{33,90}}, color={0,0,255}));
  connect(sTATCOM3_1.p, line_8_9.p) annotation (Line(points={{12,72.6},{12,90},{33,90}}, color={0,0,255}));
  connect(B9.p, pwFault2.p) annotation (Line(points={{80,90},{59,90},{59,75.1667}}, color={0,0,255}));
  annotation (
    Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-180,-120},{180,120}})),
    Icon(coordinateSystem(extent={{-180,-120},{180,120}})),
    Documentation(info="<html>
<table cellspacing=\"2\" cellpadding=\"0\" border=\"0\"<tr>
<td><p>Reference</p></td>
<td><p>PSAT Manual 2.1.8</p></td>
</tr>
<tr>
<td><p>Last update</p></td>
<td><p>15/07/2015</p></td>
</tr>
<tr>
<td><p>Author</p></td>
<td><p>MAA Murad, SmarTS Lab, KTH Royal Institute of Technology</p></td>
</tr>
<tr>
<td><p>Contact</p></td>
<td><p><a href=\"mailto:luigiv@kth.se\">luigiv@kth.se</a></p></td>
</tr>
</table>
</html>"),
    experiment(
      StopTime=20,
      Interval=0.001,
      Tolerance=1e-006,
      __Dymola_fixedstepsize=0.001,
      __Dymola_Algorithm="Rkfix2"),
    __Dymola_experimentSetupOutput);
end IEEE_9_Buses_Statcom;
