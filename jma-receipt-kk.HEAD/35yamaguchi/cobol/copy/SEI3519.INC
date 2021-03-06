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
      ************************************************************
      *          ����������� COPY (CPRMF100.INC) *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  MF102-REC.
           03  MF102-KEY.
      *�������ô���ֹ�
               05  MF102-FTNJANUM         PIC  X(08).
      *�����ֹ�
               05  MF102-PTNUM            PIC  X(20).
      *������ʬ
               05  MF102-NYUGAIKBN        PIC  X(02).
      *����ǯ��
               05  MF102-SRYYM            PIC  X(06).
      *�ݸ����ֹ�
               05  MF102-HKNJANUM         PIC  X(08).
      *������ֹ�
               05  MF102-JKYSNUM          PIC  X(20).
      *���桦�ֹ�
           03  MF102-KIGO                 PIC  X(38).
           03  MF102-NUM                  PIC  X(38).
      *����
           03  MF102-SEX                  PIC  X(01).
      *̾��
           03  MF102-NAME                 PIC  X(50).
      *��ǯ����
           03  MF102-SEIGYMD.
               05  MF102-SEIG             PIC  X(01).
               05  MF102-SEIY             PIC  X(02).
               05  MF102-SEIM             PIC  X(02).
               05  MF102-SEID             PIC  X(02).
      *�ݸ��ֹ�
           03  MF102-HKNNUM               PIC  X(03).
      *�࿦��Ǥ��
           03  MF102-NINKEI               PIC  X(01).
      *���ճ��
           03  MF102-KYURATE              PIC  X(01).
      *�ü�(Ĺ��)
           03  MF102-TYOKI                PIC  X(01).
      *����ǯ����
           03  MF102-NYUINGYMD.
               05  MF102-NYUING           PIC  X(01).
               05  MF102-NYUINY           PIC  X(02).
               05  MF102-NYUINM           PIC  X(02).
               05  MF102-NYUIND           PIC  X(02).
      *���Ų�
           03  MF102-SRYKA                PIC  X(02).
      *����
           03  MF102-NISSU                PIC  9(02).
      *ʡ���������������
           03  MF102-TENSU                PIC  9(08).
      *�����о�����
           03  MF102-KESETENSU            PIC  9(08).
      *��������
           03  MF102-SHOKUJINISSU         PIC  9(02).
      *���������������ѳ�
           03  MF102-SHOKUJIRYOYOHI       PIC  9(07).
      *����������ɸ����ô��
           03  MF102-SHOKUJIFTN           PIC  9(06).
      *ʡ������������
           03  MF102-YKZFTN               PIC  9(08).
      *������ô��
           03  MF102-KESEYKZFTN           PIC  9(07).
      *ʡ������񿩻�������
           03  MF102-JOSEIGK              PIC  9(07).
      *�ݸ���ʬ
           03  MF102-HKNKBN               PIC  X(01).
      *�����ֹ�
           03  MF102-TAHONUM              PIC  X(03).
      *����ɣ�
           03  MF102-TAHOKOHID            PIC  9(10).
      *������ô���ֹ�
           03  MF102-TAHOFTNJANUM         PIC  X(08).
