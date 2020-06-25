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
      *    �ǡ��������å���֥ե�����     COPY (CPDTCHK013.INC)  *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  DTCHK013-REC.
           03  DTCHK013-KEY.
               05  DTCHK013-PTID               PIC 9(10).
               05  DTCHK013-TANICD             PIC 9(03).
               05  DTCHK013-SRYCD              PIC X(09).
               05  DTCHK013-SHONUM             PIC 9(03).
               05  DTCHK013-SRYYMD             PIC X(08).
               05  DTCHK013-SRYSTYMD           PIC X(08).
               05  DTCHK013-SRYEDYMD           PIC X(08).
               05  DTCHK013-MSTKBN             PIC X(01).
      *    ñ��̾��
           03  DTCHK013-TANINAME               PIC X(12).
      *    ñ��������β�����
           03  DTCHK013-LIMIT                  PIC 9(05).
      *    �����ñ�̥�����901�ʷ���ե꡼����ξ���
           03  DTCHK013-TSUKISU                PIC 9(02).
      *    ��������
           03  DTCHK013-TOKUREI                PIC 9(01).
      *    ñ��������β�����
           03  DTCHK013-KAISU-TOTAL            PIC 9(05).
           03  DTCHK013-DAY-G.
               05  DTCHK013-DAY                PIC 9(05)
                                               OCCURS 31.
      *    ����ǯ��ʽ��ˤβ��
           03  DTCHK013-KAISU1                 PIC 9(05).
      *    ����ǯ��ʽ��˳��β��
           03  DTCHK013-KAISU2                 PIC 9(05).
      *    ����ǯ��ʽ��˳��ǽ�������
           03  DTCHK013-OUTYMD                 PIC X(08).
           03  DTCHK013-NYUINYMD               PIC X(08).
           03  DTCHK013-TAIINYMD               PIC X(08).
           03  DTCHK013-SRYYM                  PIC X(06).
           03  DTCHK013-WEEK                   PIC 9(01).
           03  DTCHK013-JGNCNTERR              PIC 9(01).
           03  DTCHK013-NYUGAIKBN              PIC X(01).
      *    ����ǯ����ݸ�����
           03  DTCHK013-HKNCOMBI-MAX           PIC 9(03).
           03  DTCHK013-HKNCOMBI               PIC 9(04)
                                               OCCURS 40.