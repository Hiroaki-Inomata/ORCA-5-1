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
      *    �ѥ�᥿��  COPY  (CPPARA.INC)               *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    ¿��Ǽ       02/07/30  ü���ɣĤ�16 ���� 32 ��
      *  01.00.02    ��ƣ         03/03/12  ü���ɣĤ�32 ���� 64 ��
      ************************************************************
      *01  PARA-REC.
           03  PARA-KEY.
               05  PARA-HOSPNUM           PIC 9(2).
      *��̳�ɣ�
               05  PARA-GYOUMUID          PIC X(04).
      *ü���ɣ�
               05  PARA-TERMID            PIC X(64).
      *�ե�����̾
               05  PARA-FILEMEI           PIC X(20).
      *����
               05  PARA-EDANUM            PIC 9(03).
      *�ǡ����ΰ�
           03  PARA-DATA-REC.
               05  PARA-DATA-REC1         PIC X(1000).
               05  PARA-DATA-REC2         PIC X(1000).
               05  PARA-DATA-REC3         PIC X(1000).
               05  PARA-DATA-REC4         PIC X(1000).
               05  PARA-DATA-REC5         PIC X(1000).
      *����ǯ����
           03  PARA-UPYMD                 PIC X(08).
      *��������
           03  PARA-UPHMS                 PIC X(06).