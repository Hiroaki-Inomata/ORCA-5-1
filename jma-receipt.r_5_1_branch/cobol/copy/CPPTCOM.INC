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
      *    ���ԥ�����       COPY (CPPTCOM.INC)
      *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 04.06.00     NACL-¿��Ǽ  11/02/14  ��ˡ�������б�
      * 05.00.00     NACL-¿��Ǽ  20/02/XX  �����ȡ������ͷ���ѹ�
      ************************************************************
      *01  PTCOM-REC.
      *����
           03  PTCOM-KEY.
      *���ŵ��أɣ�
               05  PTCOM-HOSPNUM         PIC 9(2).
      *���ԣɣ�
               05  PTCOM-PTID           PIC  9(10).
      *���ֹ�
               05  PTCOM-ZAINUM         PIC  9(08).
      *���ť�����
               05  PTCOM-SRYCD          PIC  X(09).
      *�����Ȼ����ֹ�
               05  PTCOM-RENNUM         PIC  9(03).
      *���ϥ�����
           03  PTCOM-INPUTCOMENT        PIC  X(140).
      *�������ΰ�
           03  PTCOM-INPUTCHI-AREA.
      *�����ͣ�
               05  PTCOM-INPUTCHI1      PIC  X(10).
      *�����ͣ�
               05  PTCOM-INPUTCHI2      PIC  X(10).
      *�����ͣ�
               05  PTCOM-INPUTCHI3      PIC  X(10).
      *�����ͣ�
               05  PTCOM-INPUTCHI4      PIC  X(10).
      *�����ͣ�
               05  PTCOM-INPUTCHI5      PIC  X(10).
           03  PTCOM-INPUTCHI-AREA-R    REDEFINES   PTCOM-INPUTCHI-AREA
                                        OCCURS  5.
      *������
               05  PTCOM-INPUTCHI       PIC  X(10).
      *ü���ɣ�
           03  PTCOM-TERMID             PIC  X(16).
      *���ڡ��졼���ɣ�
           03  PTCOM-OPID               PIC  X(16).
      *����ǯ����
           03  PTCOM-CREYMD             PIC  X(08).
      *����ǯ����
           03  PTCOM-UPYMD              PIC  X(08).
      *��������
           03  PTCOM-UPHMS              PIC  X(06).
      *