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
      *    �쥻�ץ����ٽ�     COPY (CPRCF0855.INC)               *
      *                       ϫ�ҥ쥻�Ťδ�Ϣ�����ȡ�������   *
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *
      *  05.00.01    NACL-����    20/03/30  ���£�ǯ��������б�
      ************************************************************
      *01  RECE855-REC.
           03  RECE855-KEY.
             05  RECE855-KEY1.
      *���ŵ��أɣ�
               07  RECE855-HOSPNUM         PIC 9(02).
      *����ǯ��
               07  RECE855-SRYYM           PIC 9(06).
      *������ʬ
               07  RECE855-NYUGAIKBN       PIC X(01).
      *���ԣɣ�
               07  RECE855-PTID            PIC 9(10).
      *�쥻�ץȲ�
               07  RECE855-RECEKA          PIC X(02).
      *�ݸ��ɣ�
               07  RECE855-HKNID           PIC 9(10).
      *Ŭ�ѳ���ǯ����
               07  RECE855-TEKSTYMD        PIC X(08).
      *���ֹ�
               07  RECE855-ZAINUM          PIC 9(08).
      *Ϣ�ֹ�
               07  RECE855-RENNUM          PIC 9(02).
      *
             05  RECE855-SHORI-RENNUM      PIC 9(01).
      *
             05  RECE855-SEQNO             PIC 9(02).
      *�����Ⱦ���
           03  RECE855-SRYACTINF.
      *���ť�����
               05  RECE855-SRYCD           PIC X(09).
      *����̾��
               05  RECE855-NAME            PIC X(300).
      *���ϥ�����
               05  RECE855-INPUTCOMENT     PIC X(140).
