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
       01  SC11.
      *���ŵ��إ�����
              03  SC11-HOSPCD          PIC  X(14).
      *����ǯ��
              03  SC11-PRTYM           PIC  X(16).
      *����ǯ����
              03  SC11-SEIYMD          PIC  X(22).
      *������ʬɽ������� ��Ĵ��
              03  SC11-TENHYOKBN       PIC  X(02) OCCURS 2.
      *����
              03  SC11-ADRS            PIC  X(100).
      *���ŵ���̾
              03  SC11-HOSPNAME        PIC  X(100).
      *�����ֹ�
              03  SC11-TEL             PIC  X(15).
      *������̾
              03  SC11-KAISETUNAME     PIC  X(40).
      *NO
              03  SC11-NO              PIC  X(03).
      *
      *��ô���ֹ�
              03  SC11-FTNJA-TBL.
                  05  SC11-FTNJANUM2   PIC  X(04) OCCURS 10.
                  05  SC11-FTNJANUM4   PIC  X(08) OCCURS 10.
      *������ֹ�
              03  SC11-JKYSNUM-TBL.
                  05  SC11-JKYSNUM     PIC  X(14) OCCURS 10.
      *�ݸ����ֹ�
              03  SC11-HKNJANUM-TBL.
                  05  SC11-HKNJANUM    PIC  X(16) OCCURS 10.
      *���Ի�̾
              03  SC11-KANANAME-TBL.
                  05  SC11-KANANAME    PIC  X(20) OCCURS 10.
              03  SC11-NAME-TBL.
                  05  SC11-NAME        PIC  X(20) OCCURS 10.
      *��ô���
              03  SC11-FTNRATE-TBL.
                  05  SC11-FTNRATE-G OCCURS 10.
                      07  SC11-FTNRATE PIC X(02) OCCURS 3.
      *����������
              03  SC11-NYUGAI-TBL.
                  05  SC11-NYUGAI-G OCCURS 10.
                      07  SC11-NYUGAIKBN  PIC X(02) OCCURS 2.
      *��������
              03  SC11-NISSU-TBL.
                  05  SC11-NISSU       PIC  X(02) OCCURS 10.
      *�����
              03  SC11-SEIKYUGK-TBL.
                  05  SC11-SEIKYUGK    PIC  X(07) OCCURS 10.
      *����ɸ����ô��
              03  SC11-SFTNGK-TBL.
                  05  SC11-SFTNGK      PIC  X(07) OCCURS 10.
      *����ʬ����
              03  SC11-TENSU-TBL.
                  05  SC11-TENSU       PIC  X(07) OCCURS 10.
      *������
              03  SC11-BIKO-TBL OCCURS 10.
                  05  SC11-BIKO1       PIC  X(02) OCCURS 2.
                  05  SC11-BIKO2       PIC  X(02) OCCURS 2.
      *���
              03  SC11-GOKEI-TBL.
      *��׷��
                  05  SC11-GKENSU      PIC  X(02).
      *��������
                  05  SC11-GSEIKYUGK   PIC  X(09).
