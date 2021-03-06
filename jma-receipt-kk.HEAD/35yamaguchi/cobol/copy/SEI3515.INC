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
      *01  MF101-REC.
           03  MF101-KEY.
      *�������ô���ֹ�
               05  MF101-FTNJANUM         PIC  X(08).
      *�����ֹ�
               05  MF101-PTNUM            PIC  X(20).
      *������ʬ
               05  MF101-NYUGAIKBN        PIC  X(02).
      *����ǯ��
               05  MF101-SRYYM            PIC  X(06).
      *�ݸ����ֹ�
               05  MF101-HKNJANUM         PIC  X(08).
      *������ֹ�
               05  MF101-JKYSNUM          PIC  X(20).
      *���桦�ֹ�
           03  MF101-KIGO                 PIC  X(06).
           03  MF101-NUM                  PIC  X(10).
      *����
           03  MF101-SEX                  PIC  X(01).
      *̾��
           03  MF101-NAME                 PIC  X(50).
      *��ǯ����
           03  MF101-SEIGYMD.
               05  MF101-SEIG             PIC  X(01).
               05  MF101-SEIY             PIC  X(02).
               05  MF101-SEIM             PIC  X(02).
               05  MF101-SEID             PIC  X(02).
      *�ݸ��ֹ�
           03  MF101-HKNNUM               PIC  X(03).
      *�࿦��Ǥ��
           03  MF101-NINKEI               PIC  X(01).
      *���ճ��
           03  MF101-KYURATE              PIC  X(01).
      *�ü�(Ĺ��)
           03  MF101-TYOKI                PIC  X(01).
      *����ǯ����
           03  MF101-NYUINGYMD.
               05  MF101-NYUING           PIC  X(01).
               05  MF101-NYUINY           PIC  X(02).
               05  MF101-NYUINM           PIC  X(02).
               05  MF101-NYUIND           PIC  X(02).
      *���Ų�
           03  MF101-SRYKA                PIC  X(02).
      *����
           03  MF101-NISSU                PIC  9(02).
      *ʡ���������������(���)
           03  MF101-TENSU                PIC  9(08).
      *�����������
           03  MF101-KESETENSU            PIC  9(08).
      *��������
           03  MF101-SHOKUJINISSU         PIC  9(02).
      *���������������ѳ�
           03  MF101-SHOKUJIRYOYOHI       PIC  9(07).
      *����������ɸ����ô��
           03  MF101-SHOKUJIFTN           PIC  9(06).
      *���ް�����ô��
           03  MF101-YKZFTN               PIC  9(08).
      *����������ް�����ô��
           03  MF101-KESEYKZFTN           PIC  9(07).
      *ʡ������񿩻�������
           03  MF101-JOSEIGK              PIC  9(07).
      *�ݸ���ʬ
           03  MF101-HKNKBN               PIC  X(01).
