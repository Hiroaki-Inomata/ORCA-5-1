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
      ******************************************************************
      *      �Ż�����ɽ�����Υ����ȥơ��֥�ʣϣңã��ץ��������ȡ�
      *                                    COPY (CPETENSU2-SAMPLE.INC) *
      ******************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  ETENSU2SAMPLE-REC.
           03  ETNSSMP-KEY.
      *���롼���ֹ�
               05  ETNSSMP-H-GROUP             PIC  X(07).
      *���Ź԰٥�����
               05  ETNSSMP-SRYCD               PIC  X(09).
      *ͭ��������
               05  ETNSSMP-YUKOSTYMD           PIC  X(08).
      *ͭ����λ��
               05  ETNSSMP-YUKOEDYMD           PIC  X(08).
      *Ϣ��
               05  ETNSSMP-RENNUM              PIC  9(02).
      *���Υ����ȥ�����
           03  ETNSSMP-SAMPLECD            PIC  X(09).
      *����ǯ����
           03  ETNSSMP-CHGYMD              PIC  X(08).
      *������
           03  ETNSSMP-UP-KEY.
               05  ETNSSMP-UPH-GROUP       PIC  X(07).
               05  ETNSSMP-UPSRYCD         PIC  X(09).
               05  ETNSSMP-UPYUKOSTYMD     PIC  X(08).
               05  ETNSSMP-UPYUKOEDYMD     PIC  X(08).
               05  ETNSSMP-UPRENNUM        PIC  9(02).