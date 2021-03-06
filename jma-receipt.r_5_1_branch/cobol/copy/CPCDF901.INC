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
      *    �����ǡ������±������         COPY (CPCDF901.INC)    *
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  COLDAT91-REC.
           03  COLDAT91-KEY.
      *        �쥳���ɶ�ʬ
               05  COLDAT91-RECKBN         PIC  X(01).
               05  COLDAT91-RECSEQ         PIC  9(03).
           03  COLDAT91-BODY-1.
      *        ���ŵ��أɣ�
               05  COLDAT91-HOSPID         PIC  X(24).
      *        ��ƻ�ܸ�
               05  COLDAT91-PREFNUM        PIC  9(02).
      *        ���ŵ��ؼ���
               05  COLDAT91-HOSPSBT        PIC  9(01).
      *        �������±�
               05  COLDAT91-HOSPSBT1       PIC  9(01).
      *        �¾����ʵ��ġ�
               05  COLDAT91-BEDSU          PIC  9(04).
      *        �¾����ʰ��̡�
               05  COLDAT91-BEDSUIPN       PIC  9(04).
      *        ������ʬ
               05  COLDAT91-NYUGAIKBN      PIC  X(01).
      *        �ǡ���ǯ��
               05  COLDAT91-DATAYM         PIC  X(06).
      *        �ץ�����༱���ֹ�
               05  COLDAT91-PRGSKBNO       PIC  X(08).
      *
               05  COLDAT91-YOBI1          PIC  X(68).
           03  COLDAT91-BODY-2             REDEFINES
                                           COLDAT91-BODY-1.
      *        ���Ųʥ�����
               05  COLDAT91-SRYKA          PIC  X(02).
      *        �쥻�ſ��Ųʥ�����
               05  COLDAT91-RECEKA         PIC  X(02).
      *        ���Ų�̾
               05  COLDAT91-SRYKANAME      PIC  X(40).
      *
               05  COLDAT91-YOBI2          PIC  X(75).
           03  COLDAT91-BODY-3             REDEFINES
                                           COLDAT91-BODY-1.
      *        ���ߴ�ॳ����
               05  COLDAT91-SSTKIJUNCD     PIC  9(03).
      *
               05  COLDAT91-YOBI3          PIC  X(116).
           03  COLDAT91-BODY-4             REDEFINES
                                           COLDAT91-BODY-1.
      *        �ݸ��ֹ�
               05 COLDAT91-HKNNUM          PIC  X(03).
      *        ��ʧ��ʬ
               05 COLDAT91-PAYKBN          PIC  X(02).
      *        ˡ���ֹ�
               05 COLDAT91-HBTNUM          PIC  X(02).
      *        ����̾
               05 COLDAT91-SEIDONAME       PIC  X(100).
      *        �ݸ�������̶�ʬ
               05 COLDAT91-HKNKOHSBTKBN    PIC  X(01).
      *        ������ݶ�ʬ
               05 COLDAT91-HKNKOHKBN       PIC  X(01).
      *        ����ǯ�𳫻�
               05 COLDAT91-JOKEN-STYEAR    PIC  9(03).
      *        ����ǯ��λ
               05 COLDAT91-JOKEN-EDYEAR    PIC  9(03).
      *        ����ñ��
               05 COLDAT91-TENTNK          PIC  9(03).
      *        �쥻�ץ������ʬ
               05 COLDAT91-RECESKYKBN      PIC  X(01).
