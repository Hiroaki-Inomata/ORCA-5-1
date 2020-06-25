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
      *                         COPY (CMTIMEKSN2016.INC)               *
      *----------------------------------------------------------------*
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
      *    ���ֳ��û��ơ��֥�
       01  TBL-TIMEKSN-AREA.
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
      *����������Ĥ����Ųʡʽ�ǡ�
      *    ���ֳ�
           03  FILLER              PIC X(14)   VALUE   "1109 113020170".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "1209 113020270".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "1309 113020370".
      *    ���ֳ�����
           03  FILLER              PIC X(14)   VALUE   "1409 113020470".
      *    ��֡ʻ��ֳ��ˡʾ�����
           03  FILLER              PIC X(14)   VALUE   "1509 113020570".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "1609 113020670".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "1709 113020770".
      *����������Ĥ����Ųʡʺƿǡ�
      *    ���ֳ�
           03  FILLER              PIC X(14)   VALUE   "2110 113020870".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "2210 113020970".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "2310 113021070".
      *    ���ֳ�����
           03  FILLER              PIC X(14)   VALUE   "2410 113021170".
      *    ��֡ʻ��ֳ��˲û�
           03  FILLER              PIC X(14)   VALUE   "2510 113021270".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "2610 113021370".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "2710 113021470".
      *����������Ĥ����Ų�(������Ųʡ�
      *    ���ֳ�
           03  FILLER              PIC X(14)   VALUE   "2111 113021570".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "2211 113021670".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "2311 113021770".
      *    ���ֳ�����
           03  FILLER              PIC X(14)   VALUE   "2411 113021870".
      *    ��֡ʻ��ֳ��˲û�
           03  FILLER              PIC X(14)   VALUE   "2511 113021970".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "2611 113022070".
      *    ����
           03  FILLER              PIC X(14)   VALUE   "2711 113022170".
      *
       01  TBL-TIMEKSN-AREA-R      REDEFINES   TBL-TIMEKSN-AREA.
           03  TBL-TIMEKSN-OCC     OCCURS   94
                                   INDEXED  TBL-TIDX.
      *        1:��ǡ�2���ƿǡ�3:�ϰ�����������4��ǧ�ξ��ϰ�
               05  TBL-TM-KBN1         PIC X(01).
      *        1:���ֳ���2��������3:���롢4:������ֳ���5:��֡�8:��֡���ī
               05  TBL-TM-TIMKBN       PIC X(01).
      *        01:���Ļ��ʽ�ǡˡ�02:���Ļ��ʺƿǡˡ�03:���Ļ��ʳ����������
      *        04:�����ʡʽ�ǡˡ�05:�����ʡʺƿǡˡ�06:�����ʡʳ����������
      *        07:���Ļ����ϰ����ˡ�08:���Ļ���ǧ�ξɡ�
      *        09:����������ʽ�ǡˡ�10:����������ʺƿǡ�
      *        11:����������ʳ����������
               05  TBL-TM-KBN2         PIC X(02).
      *        G:���������
               05  TBL-TM-KBN3         PIC X(01).
      *
               05  TBL-TM-SRYCD        PIC X(09).
      *
      *