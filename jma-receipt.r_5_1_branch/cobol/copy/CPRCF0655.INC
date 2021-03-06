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
      *    �쥻�ץ����ٽ�     COPY (CPRCF0655.INC)               *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.06.01    NACL-ƣ��    10/10/12  ����ñ�ȤΥ쥻ʬΥ�б�
      *                                     ������kohid �ɲ�
      *
      *  04.07.01    NACL-ƣ��    12/12/17  ʣ����쥻�ץ�ʬ���б�
      *
      *  05.00.01    NACL-����    20/03/30  ���£�ǯ��������б�
      ************************************************************
      *01  RECE655-REC.
           03  RECE655-KEY.
             05  RECE655-KEY1.
      *���ŵ��أɣ�
               07  RECE655-HOSPNUM         PIC 9(2).
      *����ǯ��
               07  RECE655-SRYYM           PIC 9(06).
      *������ʬ
               07  RECE655-NYUGAIKBN       PIC X(01).
      *���ԣɣ�
               07  RECE655-PTID            PIC 9(10).
      *�쥻�ץȲ�
               07  RECE655-RECEKA          PIC X(02).
      *������ʧ����
               07  RECE655-TEISYUTUSAKI    PIC 9(01).
      *�쥻�ץȼ���
               07  RECE655-RECESYUBETU     PIC 9(04).
      *�ݸ����ֹ�        
               07  RECE655-HKNJANUM        PIC X(08).
      *�����ʬ
               07  RECE655-HOJOKBN         PIC X(01).
      *����ɣ�
               07  RECE655-KOHID           PIC 9(10).
      *Ŭ�ѳ���ǯ����
               07  RECE655-TEKSTYMD        PIC X(08).
      *ʬ���ʬ
               07  RECE655-RECEIPT-KBN     PIC X(10).
      *���ֹ�
               07  RECE655-ZAINUM          PIC 9(08).
      *��Ϣ��
               07  RECE655-ZAIREN          PIC 9(02).
      *Ϣ�ֹ�
               07  RECE655-RENNUM          PIC 9(02).
      *
             05  RECE655-SHORI-RENNUM      PIC 9(01).
      *
             05  RECE655-SEQNO             PIC 9(02).
      *�����Ⱦ���
           03  RECE655-SRYACTINF.
      *���ť�����
               05  RECE655-SRYCD           PIC X(09).
      *����̾��
               05  RECE655-NAME            PIC X(300).
      *���ϥ�����
               05  RECE655-INPUTCOMENT     PIC X(140).
