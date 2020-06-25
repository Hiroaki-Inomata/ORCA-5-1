      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *----------------------------------------------------------------*
      *    ���ֳ��û������ɥơ��֥롡ʿ������ǯ�����������            *
      *                         COPY (CMTIMEKSN2018.INC)               *
      *----------------------------------------------------------------*
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
      *    ���ֳ��û��ơ��֥�
       01  TBL-TIMEKSN18-AREA.
      *���
      *    ���ֳ�
           03  FILLER              PIC X(14)   VALUE   "11   111000570".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "12   111000670".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "13   111000770".
      *    ���ֳ�����
           03  FILLER              PIC X(14)   VALUE   "14   111000870".
      *    ��ī�û�
           03  FILLER              PIC X(14)   VALUE   "18   111012470".
      *�����Ļ��� ���ֳ�
           03  FILLER              PIC X(14)   VALUE   "1101 111011970".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "1201 111012070".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "1301 111012170".
      *    ���ֳ�����
           03  FILLER              PIC X(14)   VALUE   "1401 111012270".
      *    ��֡ʻ��ֳ��˲û�
           03  FILLER              PIC X(14)   VALUE   "1501 111011570".
      *    �����û�
           03  FILLER              PIC X(14)   VALUE   "1601 111011670".
      *    ����û�
           03  FILLER              PIC X(14)   VALUE   "1701 111011770".
      *�ƿ����û�
      *    ���ֳ�
           03  FILLER              PIC X(14)   VALUE   "21   112001110".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "22   112001210".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "23   112001310".
      *    ���ֳ�����
           03  FILLER              PIC X(14)   VALUE   "24   112001410".
      *    ��ī�û�
           03  FILLER              PIC X(14)   VALUE   "28   112015570".
      *�����Ļ���    ���ֳ�
           03  FILLER              PIC X(14)   VALUE   "2102 112014770".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "2202 112014870".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "2302 112014970".
      *    ���ֳ�����
           03  FILLER              PIC X(14)   VALUE   "2402 112015070".
      *    ��֡ʻ��ֳ��˲û�
           03  FILLER              PIC X(14)   VALUE   "2502 112014170".
      *    �����û�
           03  FILLER              PIC X(14)   VALUE   "2602 112014270".
      *    ����û�
           03  FILLER              PIC X(14)   VALUE   "2702 112014370".
      *����ǻ����û�
      *    ���ֳ�
           03  FILLER              PIC X(14)   VALUE   "21  G112006470".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "22  G112006570".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "23  G112006670".
      *    ���ֳ�����
           03  FILLER              PIC X(14)   VALUE   "24  G112006770".
      *�����Ļ���    ���ֳ�
           03  FILLER              PIC X(14)   VALUE   "2103 112015170".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "2203 112015270".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "2303 112015370".
      *    ���ֳ�����
           03  FILLER              PIC X(14)   VALUE   "2403 112015470".
      *    ��֡ʻ��ֳ��˲û�
           03  FILLER              PIC X(14)   VALUE   "2503 112014470".
      *    �����û�
           03  FILLER              PIC X(14)   VALUE   "2603 112014570".
      *    ����û�
           03  FILLER              PIC X(14)   VALUE   "2703 112014670".
      *�����ʳ�����Ųʡʽ�ǡ�
      *    ���ֳ�
           03  FILLER              PIC X(14)   VALUE   "1104 113009670".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "1204 113007170".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "1304 113007270".
      *    ���ֳ�����
           03  FILLER              PIC X(14)   VALUE   "1404 111010770".
      *    ��֡ʻ��ֳ��˲û�
           03  FILLER              PIC X(14)   VALUE   "1504 113007070".
      *    �����û�
           03  FILLER              PIC X(14)   VALUE   "1604 113007170".
      *    ����û�
           03  FILLER              PIC X(14)   VALUE   "1704 113007270".
      *�����ʳ�����Ųʡʺƿǡ�
      *    ���ֳ�
           03  FILLER              PIC X(14)   VALUE   "2105 113009770".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "2205 113007470".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "2305 113007570".
      *    ���ֳ�����
           03  FILLER              PIC X(14)   VALUE   "2405 112006070".
      *    ��֡ʻ��ֳ��˲û�
           03  FILLER              PIC X(14)   VALUE   "2505 113007370".
      *    �����û�
           03  FILLER              PIC X(14)   VALUE   "2605 113007470".
      *    ����û�
           03  FILLER              PIC X(14)   VALUE   "2705 113007570".
      *�����ʳ�����Ųʡʳ�����Ųʡ�
      *    ���ֳ�
           03  FILLER              PIC X(14)   VALUE   "2106 113009870".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "2206 113007770".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "2306 113007870".
      *    ���ֳ�����
           03  FILLER              PIC X(14)   VALUE   "2406 113005570".
      *    ��֡ʻ��ֳ��˲û�
           03  FILLER              PIC X(14)   VALUE   "2506 113007670".
      *    �����û�
           03  FILLER              PIC X(14)   VALUE   "2606 113007770".
      *    ����û�
           03  FILLER              PIC X(14)   VALUE   "2706 113007870".
      *�ϰ���������
      *    ���ֳ�
           03  FILLER              PIC X(14)   VALUE   "31   113016270".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "32   113016370".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "33   113016470".
      *    ���ֳ�����
           03  FILLER              PIC X(14)   VALUE   "34   113016870".
      *    ���Ļ����ֳ��û�
           03  FILLER              PIC X(14)   VALUE   "3107 113016570".
      *    ���Ļ������û�
           03  FILLER              PIC X(14)   VALUE   "3207 113016670".
      *    ���Ļ�����û�
           03  FILLER              PIC X(14)   VALUE   "3307 113016770".
      *    ���Ļ����ֳ�����û�
           03  FILLER              PIC X(14)   VALUE   "3407 113016870".
      *    ������ ���Ļ����
           03  FILLER              PIC X(14)   VALUE   "3507 113017070".
      *    ������ ���Ļ�����
           03  FILLER              PIC X(14)   VALUE   "3607 113017170".
      *    ������ ���Ļ�����
           03  FILLER              PIC X(14)   VALUE   "3707 113017270".
      *    ��ī�û�
           03  FILLER              PIC X(14)   VALUE   "38   113017370".
      *
      *ǧ�ξ��ϰ���������
      *    ���ֳ�
           03  FILLER              PIC X(14)   VALUE   "41   113018570".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "42   113018670".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "43   113018770".
      *    ���ֳ�����
           03  FILLER              PIC X(14)   VALUE   "44   113019170".
      *    ���Ļ����ֳ��û�
           03  FILLER              PIC X(14)   VALUE   "4108 113018870".
      *    ���Ļ������û�
           03  FILLER              PIC X(14)   VALUE   "4208 113018970".
      *    ���Ļ�����û�
           03  FILLER              PIC X(14)   VALUE   "4308 113019070".
      *    ���Ļ����ֳ�����û�
           03  FILLER              PIC X(14)   VALUE   "4408 113019270".
      *    ������ ���Ļ����
           03  FILLER              PIC X(14)   VALUE   "4508 113019370".
      *    ������ ���Ļ�����
           03  FILLER              PIC X(14)   VALUE   "4608 113019470".
      *    ������ ���Ļ�����
           03  FILLER              PIC X(14)   VALUE   "4708 113019570".
      *    ��ī�û�
           03  FILLER              PIC X(14)   VALUE   "48   113019670".
      *
      *����������Ĥ����Ųʡʽ�ǡˡ����Ļ���
      *    ���Ļ������ֳ�
           03  FILLER              PIC X(14)   VALUE   "1109 113020170".
      *    ���Ļ��������ʾ����ʡ�
           03  FILLER              PIC X(14)   VALUE   "1209 113020670".
      *    ���Ļ�������ʾ����ʡ�
           03  FILLER              PIC X(14)   VALUE   "1309 113020770".
      *    ���Ļ������ֳ�����ʾ����ʡ�
           03  FILLER              PIC X(14)   VALUE   "1409 113020470".
      *    ���Ļ�����֡ʾ����ʡ�
           03  FILLER              PIC X(14)   VALUE   "1509 113020570".
      *    ���Ļ��������ʾ����ʡ�
           03  FILLER              PIC X(14)   VALUE   "1609 113020670".
      *    ���Ļ�������ʾ����ʡ�
           03  FILLER              PIC X(14)   VALUE   "1709 113020770".
      *����������Ĥ����Ųʡʺƿǡˡ����Ļ���
      *    ���Ļ������ֳ�
           03  FILLER              PIC X(14)   VALUE   "2110 113020870".
      *    ���Ļ��������ʾ����ʡ�
           03  FILLER              PIC X(14)   VALUE   "2210 113021370".
      *    ���Ļ�������ʾ����ʡ�
           03  FILLER              PIC X(14)   VALUE   "2310 113021470".
      *    ���Ļ������ֳ�����
           03  FILLER              PIC X(14)   VALUE   "2410 113021170".
      *    ���Ļ�����֡ʾ����ʡ�
           03  FILLER              PIC X(14)   VALUE   "2510 113021270".
      *    ���Ļ��������ʾ����ʡ�
           03  FILLER              PIC X(14)   VALUE   "2610 113021370".
      *    ���Ļ�������ʾ����ʡ�
           03  FILLER              PIC X(14)   VALUE   "2710 113021470".
      *����������Ĥ����Ų�(������Ųʡˡ����Ļ���
      *    ���Ļ������ֳ�
           03  FILLER              PIC X(14)   VALUE   "2111 113021570".
      *    ���Ļ��������ʾ����ʡ�
           03  FILLER              PIC X(14)   VALUE   "2211 113022070".
      *    ���Ļ�������ʾ����ʡ�
           03  FILLER              PIC X(14)   VALUE   "2311 113022170".
      *    ���Ļ������ֳ�����
           03  FILLER              PIC X(14)   VALUE   "2411 113021870".
      *    ���Ļ�����֡ʾ����ʡ�
           03  FILLER              PIC X(14)   VALUE   "2511 113021970".
      *    ���Ļ��������ʾ����ʡ�
           03  FILLER              PIC X(14)   VALUE   "2611 113022070".
      *    ���Ļ�������ʾ����ʡ�
           03  FILLER              PIC X(14)   VALUE   "2711 113022170".
      *
      *����������Ĥ����Ųʡʽ�ǡˡʣ��Фޤǡ�
      *   ���ֳ�
           03  FILLER              PIC X(14)   VALUE   "1112 113026670".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "1212 113026770".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "1312 113026870".
      *    ���ֳ�����
           03  FILLER              PIC X(14)   VALUE   "1412 113026970".
      *����������Ĥ����Ųʡʺƿǡˡʣ��Фޤǡ�
      *    ���ֳ�
           03  FILLER              PIC X(14)   VALUE   "2113 113027070".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "2213 113027170".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "2313 113027270".
      *    ���ֳ�����
           03  FILLER              PIC X(14)   VALUE   "2413 113027370".
      *����������Ĥ����Ų�(������Ųʡˡʣ��Фޤǡ�
      *    ���ֳ�
           03  FILLER              PIC X(14)   VALUE   "2114 113027470".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "2214 113027570".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "2314 113027670".
      *    ���ֳ�����
           03  FILLER              PIC X(14)   VALUE   "2414 113027770".
      *
       01  TBL-TIMEKSN18-AREA-R      REDEFINES   TBL-TIMEKSN18-AREA.
           03  TBL-TIMEKSN18-OCC     OCCURS   113
                                     INDEXED  TBL-TIDX18.
      *        1:��ǡ�2���ƿǡ�3:�ϰ�����������4��ǧ�ξ��ϰ�
               05  TBL-TM18-KBN1         PIC X(01).
      *        1:���ֳ���2��������3:���롢4:������ֳ���5:��֡�
      *        6:���������㡢7:���������㡢8:��֡���ī
               05  TBL-TM18-TIMKBN       PIC X(01).
      *        01:���Ļ��ʽ�ǡˡ�02:���Ļ��ʺƿǡˡ�03:���Ļ��ʳ����������
      *        04:�����ʡʽ�ǡˡ�05:�����ʡʺƿǡˡ�06:�����ʡʳ����������
      *        07:���Ļ����ϰ����ˡ�08:���Ļ���ǧ�ξɡ�
      *        09:����������ʽ�ǡˡ�10:����������ʺƿǡ�
      *        11:����������ʳ����������
      *        12:����������ʣ��Сˡʽ�ǡ�
      *        13:����������ʣ��Сˡʺƿǡ�
      *        14:����������ʣ��Сˡʳ����������
               05  TBL-TM18-KBN2         PIC X(02).
      *        G:���������
               05  TBL-TM18-KBN3         PIC X(01).
      *
               05  TBL-TM18-SRYCD        PIC X(09).
      *
      *H30.4 ����
      *    ǥ�زû��ơ��֥�
       01  TBL-NINPUKSN18-AREA.
      *    ���(ǥ�زû�)
           03  FILLER              PIC X(14)   VALUE   "10   111013370".
      *    �ƿ�(ǥ�زû�)
           03  FILLER              PIC X(14)   VALUE   "20   112022070".
      *    ���������(ǥ�زû�)
           03  FILLER              PIC X(14)   VALUE   "20  G112022870".
      *ǥ�ػ��ֳ��û�
      *  ���
      *    ���ֳ��ʽ�ǡ�
           03  FILLER              PIC X(14)   VALUE   "1120 111012970".
      *    �����ʽ�ǡ�
           03  FILLER              PIC X(14)   VALUE   "1220 111013070".
      *    ����ʽ�ǡ�
           03  FILLER              PIC X(14)   VALUE   "1320 111013170".
      *    ���ֳ�����ʽ�ǡ�
           03  FILLER              PIC X(14)   VALUE   "1420 111013270".
      *    ��֡ʻ��ʡ���ǡ�
           03  FILLER              PIC X(14)   VALUE   "1520 111013470".
      *    �����ʻ��ʡ���ǡ�
           03  FILLER              PIC X(14)   VALUE   "1620 111013570".
      *    ����ʻ��ʡ���ǡ�
           03  FILLER              PIC X(14)   VALUE   "1720 111013670".
      *  �ƿ�
      *    ���ֳ��ʺƿǡ�
           03  FILLER              PIC X(14)   VALUE   "2120 112021370".
      *    �����ʺƿǡ�
           03  FILLER              PIC X(14)   VALUE   "2220 112021470".
      *    ����ʺƿǡ�
           03  FILLER              PIC X(14)   VALUE   "2320 112021570".
      *    ���ֳ�����ʺƿǡ�
           03  FILLER              PIC X(14)   VALUE   "2420 112021670".
      *    ��֡ʻ��ʡ��ƿǡ�
           03  FILLER              PIC X(14)   VALUE   "2520 112022170".
      *    �����ʻ��ʡ��ƿǡ�
           03  FILLER              PIC X(14)   VALUE   "2620 112022270".
      *    ����ʻ��ʡ��ƿǡ�
           03  FILLER              PIC X(14)   VALUE   "2720 112022370".
      *  ���������
      *    ���ֳ��ʳ����������
           03  FILLER              PIC X(14)   VALUE   "2120G112022470".
      *    �������ʳ����������
           03  FILLER              PIC X(14)   VALUE   "2220G112022570".
      *    ����ʳ����������
           03  FILLER              PIC X(14)   VALUE   "2320G112022670".
      *    ���ֳ�����ʳ����������
           03  FILLER              PIC X(14)   VALUE   "2420G112022770".
      *    ��֡ʻ��ʡ������������
           03  FILLER              PIC X(14)   VALUE   "2520G112022970".
      *    �����ʻ��ʡ������������
           03  FILLER              PIC X(14)   VALUE   "2620G112023070".
      *    ����ʻ��ʡ������������
           03  FILLER              PIC X(14)   VALUE   "2720G112023170".
      *  �ϰ���������
      *    ���ֳ��ʺƿǡˡ��ϰ�����
           03  FILLER              PIC X(14)   VALUE   "3120 113024970".
      *    �����ʺƿǡˡ��ϰ�����
           03  FILLER              PIC X(14)   VALUE   "3220 113025070".
      *    ����ʺƿǡˡ��ϰ�����
           03  FILLER              PIC X(14)   VALUE   "3320 113025170".
      *    ���ֳ�����ʺƿǡˡ��ϰ�����
           03  FILLER              PIC X(14)   VALUE   "3420 113025270".
      *    ��֡ʻ��ʡ��ʺƿǡˡ��ϰ�����
           03  FILLER              PIC X(14)   VALUE   "3520 113025370".
      *    �����ʻ��ʡ��ʺƿǡˡ��ϰ�����
           03  FILLER              PIC X(14)   VALUE   "3620 113025470".
      *    ����ʻ��ʡ��ʺƿǡˡ��ϰ�����
           03  FILLER              PIC X(14)   VALUE   "3720 113025570".
      *  ǧ�ξ��ϰ���������
      *    ���ֳ��ʺƿǡˡ�ǧ�ξ��ϰ�����
           03  FILLER              PIC X(14)   VALUE   "4120 113025870".
      *    �����ʺƿǡˡ�ǧ�ξ��ϰ�����
           03  FILLER              PIC X(14)   VALUE   "4220 113025970".
      *    ����ʺƿǡˡ�ǧ�ξ��ϰ�����
           03  FILLER              PIC X(14)   VALUE   "4320 113026070".
      *    ���ֳ�����ʺƿǡˡ�ǧ�ξ��ϰ�����
           03  FILLER              PIC X(14)   VALUE   "4420 113026170".
      *    ��֡ʻ��ʡ��ʺƿǡˡ�ǧ�ξ��ϰ�����
           03  FILLER              PIC X(14)   VALUE   "4520 113026270".
      *    �����ʻ��ʡ��ʺƿǡˡ�ǧ�ξ��ϰ�����
           03  FILLER              PIC X(14)   VALUE   "4620 113026370".
      *    ����ʻ��ʡ��ʺƿǡˡ�ǧ�ξ��ϰ�����
           03  FILLER              PIC X(14)   VALUE   "4720 113026470".
      *
       01  TBL-NINPUKSN18-AREA-R   REDEFINES   TBL-NINPUKSN18-AREA.
           03  TBL-NINPUKSN18-OCC  OCCURS   38
                                   INDEXED  TBL-NPIDX18.
      *        1:��ǡ�2���ƿǡ�3:�ϰ�����������4��ǧ�ξ��ϰ�
               05  TBL-NP18-KBN1         PIC X(01).
      *        1:���ֳ���2��������3:���롢4:������ֳ���5:��֡�
      *        6:���������7:���㿼��
      *        0:ǥ�زû�
               05  TBL-NP18-TIMKBN       PIC X(01).
      *        20:ǥ�زû�
               05  TBL-NP18-KBN2         PIC X(02).
      *        G:���������
               05  TBL-NP18-KBN3         PIC X(01).
      *
               05  TBL-NP18-SRYCD        PIC X(09).
      *
      *H30.4 ����
      *    ǥ�زû��������˥ơ��֥�
       01  TBL-NYU-NINPUKSN18-AREA.
      *  �ƿ�
      *    ���ֳ��ʺƿǡˡ�������
           03  FILLER              PIC X(14)   VALUE   "2120 112019990".
      *    �����ʺƿǡˡ�������
           03  FILLER              PIC X(14)   VALUE   "2220 112020090".
      *    ����ʺƿǡˡ�������
           03  FILLER              PIC X(14)   VALUE   "2320 112020190".
      *    ���ֳ�����ʺƿǡˡ�������
           03  FILLER              PIC X(14)   VALUE   "2420 112020290".
      *    ��֡ʻ��ʡ��ƿǡˡ�������
           03  FILLER              PIC X(14)   VALUE   "2520 112020390".
      *    �����ʻ��ʡ��ƿǡˡ�������
           03  FILLER              PIC X(14)   VALUE   "2620 112020490".
      *    ����ʻ��ʡ��ƿǡˡ�������
           03  FILLER              PIC X(14)   VALUE   "2720 112020590".
      *  ���������
      *    ���ֳ��ʳ���������ˡ�������
           03  FILLER              PIC X(14)   VALUE   "2120G112020690".
      *    �������ʳ���������ˡ�������
           03  FILLER              PIC X(14)   VALUE   "2220G112020790".
      *    ����ʳ���������ˡ�������
           03  FILLER              PIC X(14)   VALUE   "2320G112020890".
      *    ���ֳ�����ʳ���������ˡ�������
           03  FILLER              PIC X(14)   VALUE   "2420G112020990".
      *    ��֡ʻ��ʡ�����������ˡ�������
           03  FILLER              PIC X(14)   VALUE   "2520G112021090".
      *    �����ʻ��ʡ�����������ˡ�������
           03  FILLER              PIC X(14)   VALUE   "2620G112021190".
      *    ����ʻ��ʡ�����������ˡ�������
           03  FILLER              PIC X(14)   VALUE   "2720G112021290".
      *
       01  TBL-NYU-NINPUKSN18-AREA-R
                               REDEFINES   TBL-NYU-NINPUKSN18-AREA.
           03  TBL-NYU-NINPUKSN18-OCC  OCCURS   14
                                   INDEXED  TBL-NYUIDX18.
      *        2���ƿ�
               05  TBL-NYU-NP18-KBN1         PIC X(01).
      *        1:���ֳ���2��������3:���롢4:������ֳ���5:��֡�
      *        6:���������7:���㿼��
               05  TBL-NYU-NP18-TIMKBN       PIC X(01).
      *
               05  TBL-NYU-NP18-KBN2         PIC X(02).
      *        G:���������
               05  TBL-NYU-NP18-KBN3         PIC X(01).
      *
               05  TBL-NYU-NP18-SRYCD        PIC X(09).