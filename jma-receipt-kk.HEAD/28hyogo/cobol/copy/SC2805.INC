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
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������  ����        ����
      *  01.00.01    ����    2019/03/08  �������򥳥��Ⱥ��
      *****************************************************************
       01  SC05.
      *����ǯ��
         03  SC05-PRTYM          PIC  X(16).
      *����ǯ����
         03  SC05-SEIYMD         PIC  X(22).
      *������
         03  SC05-CITYNAME       PIC  X(20).
      *����
         03  SC05-ADRS           PIC  X(100).
      *���ŵ���̾
         03  SC05-HOSPNAME       PIC  X(100).
      *������̾
         03  SC05-KAISETU        PIC  X(40).
      *���ŵ��إ�����
         03  SC05-HOSPCD         PIC  X(14).
      *���
         03  SC05-FTNWARI        PIC X(02) OCCURS 3.
      *
         03  SC05-MEISAI-TBL    OCCURS 20.
      *Ĺ��
             05  SC05-TYOKI1     PIC  X(04).
             05  SC05-TYOKI2     PIC  X(02).
      *��̾
             05  SC05-NAME       PIC  X(24).
      *����ǯ��
             05  SC05-TANJOYM    PIC  X(10).
      *ˡ���ֹ�
             05  SC05-HBTNUM     PIC  X(04).
      *������ô���ֹ�
             05  SC05-FTNJANUM   PIC  X(08).
      *������ֹ�
             05  SC05-JKYSNUM    PIC  X(14).
      *����ǯ��
             05  SC05-SRYYM      PIC  X(08).
      *������ʬ
             05  SC05-NYUGAIKBN  PIC  X(02) OCCURS 2.
      *����
             05  SC05-NISSU      PIC  X(04).
      *����
             05  SC05-TENSU      PIC  X(12).
      *������ô��
             05  SC05-ITBFTN     PIC  X(10).
      *����
             05  SC05-BIKO1       PIC  X(02).
             05  SC05-BIKO2       PIC  X(02).
      *���
         03  SC05-GOKEI-TBL.
      *��׷��
             05  SC05-GKENSU     PIC  X(04).
      *�������
             05  SC05-GNISSU     PIC  X(06).
      *�������
             05  SC05-GTENSU     PIC  X(14).
      *��װ�����ô��
             05  SC05-GITBFTN    PIC  X(14).
