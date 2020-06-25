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
      *         �����ޥ���    COPY (CPTENSUPLSU.INC)     *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 03.04.00     NACL-¿��Ǽ  06/12/05  ��²���ɲ�
      * 05.01.00     NACL-¿��Ǽ  20/02/05  �桼����ñ���ɲ�
      ************************************************************
      *01  TENSUPLSU-REC.
           03  TNSP-KEY.
               05  TNSP-HOSPNUM          PIC 9(2).
               05  TNSP-SRYCD           PIC  X(09).
               05  TNSP-YUKOSTYMD       PIC  X(08).
               05  TNSP-YUKOEDYMD       PIC  X(08).
      *    ����̾��
           03  TNSP-SHOYUKOKETA         PIC  9(02).
           03  TNSP-SHONAME             PIC  X(200).
      *    �η�����ʬ
           03  TNSP-SAIKETUKBN          PIC  9(01).
      *    ���ϥ����å���ʬ
           03  TNSP-INPCHKKBN           PIC  9(01).
      *    ź�ջ���������ʬ���ʬ
           03  TNSP-TENPUYAKKBN         PIC  X(02).
      *    �����̵��ܶ�ʬ
           03  TNSP-ALLSURYOKBN         PIC  X(01).
      *    ���̴���ñ�̥�����
           03  TNSP-KANZANTANICD        PIC  X(03).
      *    ���̴���ñ��̾��
           03  TNSP-KANZANTANINAME      PIC  X(24).
      *    ���̴�����
           03  TNSP-KANZANCHI           PIC  9(05)V9(05).
      *    �桼���������²��
      *    ���������ʬ�ʣ��Τߡ�
           03  TNSP-SANTEIRRKKBN        PIC  9(01).
      *    ���²��
           03  TNSP-JGNCNT              PIC  9(03).
      *    ����²��
           03  TNSP-JGNCNT1D            PIC  9(03).
      *    ����²��
           03  TNSP-JGNCNT1W            PIC  9(03).
      *    ���顼����
           03  TNSP-JGNCNTERR           PIC  9(01).
      *    ¾���
           03  TNSP-JGNCNTETCM          PIC  9(02).
      *    ¾���²��
           03  TNSP-JGNCNTETC           PIC  9(03).
      *
      *    �����Խ���ʬ
           03  TNSP-SOURYOHENKBN           PIC  9(01).
      *    ����̾���ܶ�ʬ
           03  TNSP-IPN-KISAIKBN           PIC  9(01).
      *    ����̾����ñ��
           03  TNSP-IPN-KANZANTANICD       PIC  X(03).
      *    ����̾����ñ��̾
           03  TNSP-IPN-KANZANTANINAME     PIC  X(24).
      *    ����̾���ܴ�����
           03  TNSP-IPN-KANZANCHI          PIC  9(05)V9(05).
      *
      *    ���󸡺��������ȶ�ʬ
           03  TNSP-ZENKNSKBN              PIC  9(01).
      *
      *    �������ط�
           03  TNSP-SIPPURYO1           PIC  9(03).
           03  TNSP-SIPPUIRISU1         PIC  9(03).
           03  TNSP-SIPPURYO2           PIC  9(03).
           03  TNSP-SIPPUIRISU2         PIC  9(03).
      *R02.2
      *    �桼����ñ�̥�����
           03  TNSP-USERTANICD          PIC  X(03).
      *    �桼����ñ��̾��
           03  TNSP-USERTANINAME        PIC  X(24).
      *
           03  TNSP-TERMID              PIC  X(16).
           03  TNSP-OPID                PIC  X(16).
           03  TNSP-CREYMD              PIC  X(08).
           03  TNSP-UPYMD               PIC  X(08).
           03  TNSP-UPHMS               PIC  X(06).
      *�����祭�������ѹ���
           03  TNSP-UP-KEY.
               05  TNSP-UP-HOSPNUM       PIC 9(2).
               05  TNSP-UP-SRYCD         PIC  X(09).
               05  TNSP-UP-YUKOSTYMD     PIC  X(08).
               05  TNSP-UP-YUKOEDYMD     PIC  X(08).