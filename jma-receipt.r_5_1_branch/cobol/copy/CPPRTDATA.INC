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
      *    �����ѥǡ���     COPY (CPPRTDATA.INC)                *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-ƣ��    02/10/10  �����ȶ�ʬ�ɲ�
      *  01.00.02    NACL-ƣ��    03/03/03  �ץ��̾�ɲ�
      ************************************************************
      *01  PRTDATA-REC.
      *
           03  PRTDATA-KEY.
               05  PRTDATA-HOSPNUM   PIC 9(2).
      *�ˣţ�
               05  PRTDATA-TBL-KEY    PIC X(08).
      *Ϣ��
               05  PRTDATA-RENNUM     PIC 9(04).
      *���롼��
               05  PRTDATA-TBL-GROUP  PIC X(14).
      *UUID
               05  PRTDATA-TBL-UUID   PIC X(36).
      *������Ϣ��
               05  PRTDATA-SHORI-RENNUM
                                      PIC 9(04).
      *��̳������
               05  PRTDATA-GYOUMU-CD  PIC 9(03).
      *������ɣ�
           03  PRTDATA-SHELLID        PIC X(20).
      *Ģɼ�ɣ�
           03  PRTDATA-PRTID          PIC X(20).
      *����ǯ��
           03  PRTDATA-SRYYM          PIC X(06).
      *����ǯ����
           03  PRTDATA-SKYYMD         PIC X(08).
      *���ԣɣ�
           03  PRTDATA-PTID           PIC 9(10).
      *������Ϣ��
           03  PRTDATA-PRIORITY       PIC 9(04).
      *��
           03  PRTDATA-PAGE           PIC 9(05).
      *�����ե饰
           03  PRTDATA-PRTFLG         PIC X(01).
      *�����ȶ�ʬ
           03  PRTDATA-SITEKBN        PIC X(01).
      *�ץ��̾
           03  PRTDATA-PRTNM          PIC X(16).
      *�ǡ����μ���
           03  PRTDATA-DATA-TYPE      PIC X(16).
      *���Уɡݣɣ�
           03  PRTDATA-API-ID         PIC X(20).
      *Ģɼ�ǡ���
           03  PRTDATA-PRTDATA        PIC X(20000).
      *������
           03  PRTDATA-CREYMD         PIC X(8).
      *
      *�����ѹ���
           03  PRTDATAXX-STPAGE       PIC 9(05).
           03  PRTDATAXX-EDPAGE       PIC 9(05).    
