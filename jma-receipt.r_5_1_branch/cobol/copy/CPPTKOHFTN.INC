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
      *    ���Ը�����ô                  COPY  (CPPTKOHFTN.INC)  *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  PTKOHFTN-REC.
      *����
           03  PTKOHFTN-KEY.
      *���ŵ��أɣ�
               05  PTKOHFTN-HOSPNUM         PIC 9(2).
      *���ԣɣ�
               05  PTKOHFTN-PTID           PIC  9(10).
      *����ɣ�
               05  PTKOHFTN-KOHID          PIC  9(10).
      *Ϣ��
               05  PTKOHFTN-RENNUM         PIC  9(03).
      *Ŭ�ѳ���ǯ����
           03  PTKOHFTN-TEKSTYMD           PIC  X(08).
      *Ŭ�ѽ�λǯ����
           03  PTKOHFTN-TEKEDYMD           PIC  X(08).
      *�ݸ��ֹ�ʸ����
           03  PTKOHFTN-KOHNUM             PIC  X(03).
      *��ʧ��ʬ
           03  PTKOHFTN-PAYKBN             PIC  X(02).
      *�����³�
           03  PTKOHFTN-GAIJGNGAK          PIC  9(07).
      *������³�
           03  PTKOHFTN-NYUJGNGAK          PIC  9(07).
      *ü���ɣ�
           03  PTKOHFTN-TERMID             PIC  X(16).
      *���ڡ��졼���ɣ�
           03  PTKOHFTN-OPID               PIC  X(16).
      *����ǯ����
           03  PTKOHFTN-CREYMD             PIC  X(08).
      *����ǯ����
           03  PTKOHFTN-UPYMD              PIC  X(08).
      *��������
           03  PTKOHFTN-UPHMS              PIC  X(06).
