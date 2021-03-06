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
      *    �����������     COPY (CPSKT4600TMP.INC)            *
      *
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������  ����        ����
      *  01.00.01    ��ƣ    08/04/24    Ģɼ�ͼ���ȼ������
      *  01.00.02    ���    19/02/04    �������򥳥��Ⱥ��
      ************************************************************
       01  SKT4600TMP-REC.
           03  SKT4600TMP-KEY.
               05  SKT4600TMP-PREFKBN             PIC  X(01).
           03  SKT4600TMP-SEIKYU-TBL  OCCURS  13.
               05  SKT4600TMP-NYUGAI-TBL  OCCURS  2.
                 07  SKT4600TMP-KENSU               PIC  9(06).
                 07  SKT4600TMP-TENSU               PIC  9(10).
                 07  SKT4600TMP-YKZFTN              PIC  9(10).
                 07  SKT4600TMP-ITBFTN              PIC  9(10).
               05  SKT4600TMP-SKENSU                PIC  9(06).
               05  SKT4600TMP-SKINGK                PIC  9(10).
               05  SKT4600TMP-SFTNGK                PIC  9(10).
           03  SKT4600TMP-Z-SEIKYU-TBL  OCCURS  7.
               05  SKT4600TMP-Z-KENSU               PIC  9(06).
               05  SKT4600TMP-Z-TENSU               PIC  9(10).
               05  SKT4600TMP-Z-ITBFTN              PIC  9(10).
           03  SKT4600TMP-KOH-AREA.
               05  SKT4600TMP-KOH-TBL  OCCURS  20.
                   07  SKT4600TMP-KOHNUM            PIC  X(03).
                   07  SKT4600TMP-KOHNAME           PIC  X(10).
                   07  SKT4600TMP-KOH-NYUGAI-TBL OCCURS 2.
                       09  SKT4600TMP-K-KENSU       PIC  9(06).
                       09  SKT4600TMP-K-TENSU       PIC  9(10).
                       09  SKT4600TMP-K-YKZFTN      PIC  9(10).
                       09  SKT4600TMP-K-ITBFTN      PIC  9(10).
                   07  SKT4600TMP-K-SKENSU          PIC  9(06).
                   07  SKT4600TMP-K-SKINGK          PIC  9(10).
                   07  SKT4600TMP-K-SFTNGK          PIC  9(10).
           03  SKT4600TMP-ZKOH-AREA.
               05  SKT4600TMP-ZKOH-TBL  OCCURS  20.
                   07  SKT4600TMP-ZKOHNUM           PIC  X(03).
                   07  SKT4600TMP-ZKOHNAME          PIC  X(10).
                   07  SKT4600TMP-ZK-KENSU          PIC  9(06).
                   07  SKT4600TMP-ZK-ITBFTN         PIC  9(10).
           03  SKT4600TMP-GENBK                     PIC  9(06).
           03  SKT4600TMP-TYOKI                     PIC  9(06).
           03  SKT4600TMP-TYOKI2                    PIC  9(06).
           03  SKT4600TMP-DAISAN                    PIC  9(06).
           03  SKT4600TMP-HKNJA-KENSU               PIC  9(06).
           03  SKT4600TMP-HKNJA-KENSUK              PIC  9(06).
