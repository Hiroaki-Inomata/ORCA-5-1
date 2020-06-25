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
      *      �Ż�����ɽ������ޥ�����   COPY (CPETENSU1.INC) *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  ETENSU1-REC.
           03  ETENSU1-KEY.
      *        ���Ź԰٥�����
               05  ETENSU1-SRYCD               PIC  X(09).
      *        ͭ��������
               05  ETENSU1-YUKOSTYMD           PIC  X(08).
      *        ͭ����λ��
               05  ETENSU1-YUKOEDYMD           PIC  X(08).
      *    ����Ϣ
           03  ETENSU1-HKTGRP-AREA.
               05  ETENSU1-HKTGRP-T            OCCURS   3.
      *            ���ñ��
                   07  ETENSU1-H-TANI          PIC 9(02).
      *            ���롼���ֹ�
                   07  ETENSU1-H-GROUP         PIC X(07).
      *    ��ȿ��Ϣ����
           03  ETENSU1-HAIHAN-KANREN.
      *        �����ˤĤ�
               05  ETENSU1-R-DAY               PIC  9(01).
      *        Ʊ�����
               05  ETENSU1-R-MONTH             PIC  9(01).
      *        Ʊ��
               05  ETENSU1-R-SAME              PIC  9(01).
      *        �����֤ˤĤ�
               05  ETENSU1-R-WEEK              PIC  9(01).
      *    ��������������
           03  ETENSU1-N-GROUP                 PIC  9(03).
      *    ������
           03  ETENSU1-C-KAISU                 PIC  9(01).
      *    ����ǯ����
           03  ETENSU1-CHGYMD                  PIC  X(08).
      *������
           03  ETENSU1-UP-KEY.
               05  ETENSU1-UPSRYCD             PIC  X(09).
               05  ETENSU1-UPYUKOSTYMD         PIC  X(08).
               05  ETENSU1-UPYUKOEDYMD         PIC  X(08).