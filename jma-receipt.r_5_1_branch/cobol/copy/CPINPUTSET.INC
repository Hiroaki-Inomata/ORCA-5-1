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
      *    ���ϥ��å�       COPY (CPINPUTSET.INC)
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.05.00    NACL-¿��Ǽ  09/10/02  ���϶�ʬ���������Ͽ����ɲ�
      *  05.00.00    NACL-¿��Ǽ  20/02/XX  �����ȡ������ͷ���ѹ�
      ************************************************************
      *01 INPUTSET-REC.
           03 ISET-KEY.
      *���ŵ��أɣ�
               05 ISET-HOSPNUM          PIC 9(2).
      *���åȥ�����
               05 ISET-SETCD           PIC  X(06).
      *ͭ������ǯ����
               05 ISET-YUKOSTYMD       PIC  X(08).
      *ͭ����λǯ����
               05 ISET-YUKOEDYMD       PIC  X(08).
      *���å����ֹ�
               05 ISET-SETSEQ          PIC  9(03).
      *���ϡʿ��š˥�����
           03 ISET-INPUTCD             PIC  X(09).
      *���̣�
           03 ISET-SURYO1              PIC  9(07)V9(5).
      *���̣�
           03 ISET-SURYO2              PIC  9(07)V9(5).
      *���
           03 ISET-KAISU               PIC  9(03).
      *���ϥ�����
           03 ISET-COMMENT             PIC  X(140).
      *������
           03 ISET-ATAI-AREA.
      *�����ͣ�
               05 ISET-ATAI1           PIC  X(10).
      *�����ͣ�
               05 ISET-ATAI2           PIC  X(10).
      *�����ͣ�
               05 ISET-ATAI3           PIC  X(10).
      *�����ͣ�
               05 ISET-ATAI4           PIC  X(10).
      *�����ͣ�
               05 ISET-ATAI5           PIC  X(10).
           03  ISET-ATAI-AREA-R        REDEFINES    ISET-ATAI-AREA.
               05 ISET-ATAI            PIC  X(10)   OCCURS  5.
      *���϶�ʬ�ʣ����ط������Ȼؼ�����������������ؼ���
           03 ISET-INPUTKBN            PIC  X(01).
      *�������Ͽ���
           03 ISET-KANSURYO            PIC  9(05)V9(5).
      *ü���ɣ�
           03 ISET-TERMID              PIC  X(16).
      *���ڥ졼���ɣ�
           03 ISET-OPID                PIC  X(16).
      *����ǯ����
           03 ISET-CREYMD              PIC  X(08).
      *����ǯ����
           03 ISET-UPYMD               PIC  X(08).
      *��������
           03 ISET-UPHMS               PIC  X(06).