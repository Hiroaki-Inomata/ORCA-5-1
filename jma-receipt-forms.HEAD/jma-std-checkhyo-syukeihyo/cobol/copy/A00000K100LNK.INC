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
      *******************************************************************
      *    ���Ź԰ٶ�ʬ�̽���ɽ���֥ץ������ ���������
      *******************************************************************
      *  �ץ�����ཤ������
      *  Maj/Min/Rev  ������      ����      ����
      *   01.00.01    @@@@-����   10/12/14  ������������
      *   02.00.01    @@@@-���   18/02/01  ���ɽ����ǽ���ɲ�
      *******************************************************************
       01  A00000K100-LNK.
           03  K100LNK-PRTKANRI-RENNUM              PIC 9(04).
           03  K100LNK-PRTKANRI-TBL-KEY             PIC X(08).
           03  K100LNK-PRTKANRI-TBL-GROUP           PIC X(14).
           03  K100LNK-PRTKANRI-SHORI-RENNUM        PIC 9(04).
           03  K100LNK-PRTKANRI-SRYYM               PIC X(06).
           03  K100LNK-PRTKANRI-SKYYMD              PIC X(08).
           03  K100LNK-PRTKANRI-SHELLID             PIC X(20).
           03  K100LNK-PRTKANRI-PRIORITY            PIC 9(04).
           03  K100LNK-PRTKANRI-TERMID              PIC X(64).
           03  K100LNK-PRTKANRI-OPID                PIC X(16).
           03  K100LNK-PRTKANRI-PRTNM               PIC X(16).
           03  K100LNK-JOBID                        PIC 9(07).
           03  K100LNK-SHELLID                      PIC X(08).
           03  K100LNK-HOSPNUM                      PIC 9(02).
           03  K100LNK-SRYYM                        PIC X(08).
           03  K100LNK-NYUGAIKBN                    PIC X(01).
           03  K100LNK-HKNTG                        PIC X(01).
           03  K100LNK-HOUKATU                      PIC X(01).
           03  K100LNK-TKKBN                        PIC X(01).
