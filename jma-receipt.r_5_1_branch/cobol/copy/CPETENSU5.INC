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
      *      �Ż�����ɽ���������ơ��֥� COPY (CPETENSU5.INC) *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  ETENSU5-REC.
           03  ETENSU5-KEY.
      *        ���Ź԰٥�����
               05  ETENSU5-SRYCD               PIC  X(09).
      *        ͭ��������
               05  ETENSU5-YUKOSTYMD           PIC  X(08).
      *        ͭ����λ��
               05  ETENSU5-YUKOEDYMD           PIC  X(08).
      *        ����ñ�̥�����
               05  ETENSU5-TANICD              PIC  9(03).
      *    ����ñ��̾��
           03  ETENSU5-TANINAME                PIC  X(12).
      *    ������
           03  ETENSU5-KAISU                   PIC  9(03).
      *    ������
           03  ETENSU5-TOKUREI                 PIC  9(01).
      *    ����ǯ����
           03  ETENSU5-CHGYMD                  PIC  X(08).
      *������
           03  ETENSU5-UP-KEY.
               05  ETENSU5-UPSRYCD             PIC  X(09).
               05  ETENSU5-UPYUKOSTYMD         PIC  X(08).
               05  ETENSU5-UPYUKOEDYMD         PIC  X(08).
               05  ETENSU5-UPTANICD            PIC  9(03).