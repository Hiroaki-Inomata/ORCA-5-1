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
      *      �Ż�����ɽ����ȿ��Ϣ�ơ��֥�
      *                                     COPY (CPETENSU3.INC) *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  ETENSU3-REC.
      *
           03  ETENSU3-KEY.
      *���Ź԰٥����ɣ�
               05  ETENSU3-SRYCD1             PIC  X(09).
      *���Ź԰٥����ɣ�
               05  ETENSU3-SRYCD2             PIC  X(09).
      *ͭ��������
               05  ETENSU3-YUKOSTYMD          PIC  X(08).
      *ͭ����λ��
               05  ETENSU3-YUKOEDYMD          PIC  X(08).
      *��ȿ��ʬ
           03  ETENSU3-HAIHAN              PIC  9(01).
      *������
           03  ETENSU3-TOKUREI             PIC  9(01).
      *����ǯ����
           03  ETENSU3-CHGYMD              PIC  X(08).
      *������
           03  ETENSU3-UP-KEY.
               05  ETENSU3-UPSRYCD1        PIC  X(09).
               05  ETENSU3-UPSRYCD2        PIC  X(09).
               05  ETENSU3-UPYUKOSTYMD     PIC  X(08).
               05  ETENSU3-UPYUKOEDYMD     PIC  X(08).
      *