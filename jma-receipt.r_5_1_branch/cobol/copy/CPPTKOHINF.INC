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
      *    ���Ը������    �� COPY  (CPPTKOHINF.INC)  *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-¿��Ǽ  02/09/09  ��ʧ��ʬ�ɲ�
      ************************************************************
      *01  PTKOHINF-REC.
      *����
           03  PTKOH-KEY.
      *���ŵ��أɣ�
               05  PTKOH-HOSPNUM            PIC 9(2).
      *���ԣɣ�
               05  PTKOH-PTID              PIC  9(10).
      *����ɣ�
               05  PTKOH-KOHID             PIC  9(10).
      *�ݸ��ֹ�ʸ����
           03  PTKOH-KOHNUM                PIC  X(03).
      *��ʧ��ʬ
           03  PTKOH-PAYKBN                PIC  X(02).
      *��ô���ֹ�
           03  PTKOH-FTNJA-G.
      *��ô���ֹ�
               05  PTKOH-FTNJANUM          PIC  X(08).
      *������ֹ�
           03  PTKOH-JKYSNUM               PIC  X(20).
      *Ŭ�ѳ���ǯ����
           03  PTKOH-TEKSTYMD              PIC  X(08).
      *Ŭ�ѽ�λǯ����
           03  PTKOH-TEKEDYMD              PIC  X(08).
      *��ǧǯ����
           03  PTKOH-KAKUNINYMD            PIC  X(08).
      *�����ʬ
           03  PTKOH-SAKJOKBN              PIC  X(01).
      *�����ڤ��ʬ
           03  PTKOH-TEKEDCHKKBN           PIC  X(01).
      *ü���ɣ�
           03  PTKOH-TERMID                PIC  X(16).
      *���ڡ��졼���ɣ�
           03  PTKOH-OPID                  PIC  X(16).
      *����ǯ����
           03  PTKOH-CREYMD                PIC  X(08).
      *����ǯ����
           03  PTKOH-UPYMD                 PIC  X(08).
      *��������
           03  PTKOH-UPHMS                 PIC  X(06).
