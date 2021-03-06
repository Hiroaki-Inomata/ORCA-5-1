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
      *    �����������     COPY (CPSKT3500TMP.INC)            *
      *
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    Ĺ��   08/04/28      Ģɼ�ͼ���ȼ������
      *  01.00.02    ����   09/08/25      ���彣�����뱫�б����оݼ��̽��ס�
      *  01.00.03    ����   11/04/19      ����������ʿ�β��Ͽ��б�
      *  01.00.04    ����   19/01/11      �������򥳥��Ⱥ��
      ************************************************************
       01  SKT3510TMP-REC.
           03      SKT3510TMP-KEY.
             05    SKT3510TMP-SYUTURYOKU            PIC  9(01).
             05    SKT3510TMP-ZAITAKU               PIC  X(01).
             05    SKT3510TMP-HKNJANUM              PIC  X(06).
             05    SKT3510TMP-SRYYM                 PIC  9(06).
             05    SKT3510TMP-SRYKA                 PIC  X(02).
      *    ���彣�����뱫����
             05    SKT3510TMP-GENMEN                PIC  X(05).
           03    SKT3510TMP-PAGE-FLG1               PIC  9(01).
           03    SKT3510TMP-PAGE-FLG2               PIC  9(01).
           03    SKT3510TMP-SEIKYU-TBL              OCCURS 11.
               05    SKT3510TMP-NYUGAI-TBL          OCCURS 2.
                 07    SKT3510TMP-KENSU             PIC 9(05).
                 07    SKT3510TMP-NISSU             PIC 9(05).
                 07    SKT3510TMP-TENSU             PIC 9(09).
                 07    SKT3510TMP-ITBFTN            PIC 9(09).
               05    SKT3510TMP-SKENSU              PIC 9(05).
               05    SKT3510TMP-SNISSU              PIC 9(05).
               05    SKT3510TMP-SKINGK              PIC 9(09).
               05    SKT3510TMP-SFTNGK              PIC 9(09).
           03  SKT3510TMP-KOH-AREA.
               05  SKT3510TMP-KOH-TBL               OCCURS  20.
                   07  SKT3510TMP-KOHNUM            PIC  X(03).
                   07  SKT3510TMP-K-NYUGAI-TBL      OCCURS  2.
                       09  SKT3510TMP-K-KENSU       PIC  9(05).
                       09  SKT3510TMP-K-NISSU       PIC  9(05).
                       09  SKT3510TMP-K-TENSU       PIC  9(09).
                       09  SKT3510TMP-K-ITBFTN      PIC  9(09).
                   07  SKT3510TMP-K-SKENSU          PIC  9(05).
                   07  SKT3510TMP-K-SNISSU          PIC  9(05).
                   07  SKT3510TMP-K-SKINGK          PIC  9(09).
                   07  SKT3510TMP-K-SFTNGK          PIC  9(09).
