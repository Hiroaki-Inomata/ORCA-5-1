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
      *-------------------------------------------------*
      *    Ϸ�͡����̥����ɿ���    COPY (CPSRUCDCHG.INC)
      *
      *-------------------------------------------------*
      *01  SRYCDCHG-REC.
           03  CHG-KEY.
               05  CHG-HOSPNUM         PIC 9(2).
      *���̿��ť�����
               05  CHG-IPNSRYCD        PIC X(09).
      *Ϸ�Ϳ��ť�����
               05  CHG-RJNSRYCD        PIC X(09).
      *ü���ɣ�
           03  CHG-TERMID              PIC X(16).
      *���ڥ졼���ɣ�
           03  CHG-OPID                PIC X(16).
      *����ǯ����
           03  CHG-CREYMD              PIC X(08).
      *����ǯ����
           03  CHG-UPYMD               PIC X(08).
      *��������
           03  CHG-UPHMS               PIC X(06).
      *�祭��������
           03  CHG-UPKEY.
               05  CHG-UPHOSPNUM       PIC 9(2).
               05  CHG-UPIPNSRYCD      PIC X(09).
               05  CHG-UPRJNSRYCD      PIC X(09).