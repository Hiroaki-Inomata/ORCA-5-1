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
      *----------------------------------------------------------*
      *    �����ղþ��󹹿��ѥ�᥿        COPY (CPORCGZS02.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *
       01  ORCGZS02AREA.
      *    ����
           03  ZS02-NAI-YUKOSTYMD      PIC X(08).
           03  ZS02-NAI-YUKOEDYMD      PIC X(08).
      *    ����������
           03  ZS02-YUKOEDYMD          PIC X(08).
      *
      **** ����̾��
      *    03  ZS02-SHOYUKOKETA        PIC  9(02).
      *    03  ZS02-SHONAME            PIC  X(200).
      *    �η�����ʬ
      *    03  ZS02-SAIKETUKBN         PIC  9(01).
      *    ���ϥ����å���ʬ
      *    03  ZS02-INPCHKKBN          PIC  9(01).
      *    ʬ���ʬ
      *    03  ZS02-TENPUYAKKBN        PIC  X(02).
      *
      *    �����̵��ܶ�ʬ
      *    03  ZS02-ALLSURYOKBN        PIC  X(01).
      *    ���̴���ñ�̥�����
      *    03  ZS02-KANZANTANICD       PIC  X(03).
      *    ���̴���ñ��̾��
      *    03  ZS02-KANZANTANINAME     PIC  X(24).
      *    ���̴�����
      **** 03  ZS02-KANZANCHI          PIC  9(05)V9(05).
      *