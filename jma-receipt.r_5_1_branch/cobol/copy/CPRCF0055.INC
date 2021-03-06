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
      *    �쥻�ץ����ٽ�     COPY (CPRCF0055.INC)               *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.06.01    NACL-ƣ��    10/10/12  ����ñ�ȤΥ쥻ʬΥ�б�
      *                                     ������kohid �ɲ�
      *  04.06.02    NACL-ƣ��    12/10/26  �õ����ࣰ����Ϸʻ�˵ڤ�
      *                                     ������Ϸ��ˤΥ쥻�ץ��б�
      *
      *  05.00.01    NACL-����    20/03/30  ���£�ǯ��������б�
      ************************************************************
      *01  RECE055-REC.
           03  RECE055-KEY.
             05  RECE055-KEY1.
      *���ŵ��أɣ�
               07  RECE055-HOSPNUM         PIC 9(2).
      *����ǯ��
               07  RECE055-SRYYM           PIC 9(06).
      *������ʬ
               07  RECE055-NYUGAIKBN       PIC X(01).
      *���ԣɣ�
               07  RECE055-PTID            PIC 9(10).
      *�쥻�ץȲ�
               07  RECE055-RECEKA          PIC X(02).
      *������ʧ����
               07  RECE055-TEISYUTUSAKI    PIC 9(01).
      *�쥻�ץȼ���
               07  RECE055-RECESYUBETU     PIC 9(04).
      *�ݸ����ֹ�        
               07  RECE055-HKNJANUM        PIC X(08).
      *�����ʬ
               07  RECE055-HOJOKBN         PIC X(01).
      *����ɣ�
               07  RECE055-KOHID           PIC 9(10).
      *Ŭ�ѳ���ǯ����
               07  RECE055-TEKSTYMD        PIC X(08). 
      *ʬ���ʬ
               07  RECE055-RECEIPT-KBN     PIC X(10).
      *���ֹ�
               07  RECE055-ZAINUM          PIC 9(08).
      *Ϣ�ֹ�
               07  RECE055-RENNUM          PIC 9(02).
      *
             05  RECE055-SHORI-RENNUM      PIC 9(01).
      *
             05  RECE055-SEQNO             PIC 9(02).
      *�����Ⱦ���
           03  RECE055-SRYACTINF.
      *���ť�����
               05  RECE055-SRYCD           PIC X(09).
      *����̾��
               05  RECE055-NAME            PIC X(300).
      *���ϥ�����
               05  RECE055-INPUTCOMENT     PIC X(140).
