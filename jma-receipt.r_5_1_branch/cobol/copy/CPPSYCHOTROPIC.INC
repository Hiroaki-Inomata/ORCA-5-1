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
      ***************************************************************
      *      ����������ʬ�����ɥơ��֥�   COPY (CPPSYCHOTROPIC.INC) *
      ***************************************************************
      ***************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ***************************************************************
      *01  PSYCHOTROPIC-REC.
           03  PSYCHOTROPIC-KEY.
      *        ������
               05  PSYCHOTROPIC-CODE             PIC  X(07).
      *        ͭ��������
               05  PSYCHOTROPIC-YUKOSTYMD        PIC  X(08).
      *        ͭ����λ��
               05  PSYCHOTROPIC-YUKOEDYMD        PIC  X(08).
      *    ����������ʬ
           03  PSYCHOTROPIC-CLASS                PIC  X(01).
      *    ���롼�ץ�����
           03  PSYCHOTROPIC-GROUPCODE            PIC  X(07).
      *    ����ǯ����
           03  PSYCHOTROPIC-UPYMD                PIC  X(08).
      *������
           03  PSYCHOTROPIC-UP-KEY.
               05  PSYCHOTROPIC-UPCODE              PIC  X(07).
               05  PSYCHOTROPIC-UPYUKOSTYMD         PIC  X(08).
               05  PSYCHOTROPIC-UPYUKOEDYMD         PIC  X(08).