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
      *    ϫ�ҥ쥻�ץ����ٽ��������                            *
      *            ����̾���ؤ���   COPY (CPRCF0831.INC)         *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-ƣ��    04/02/01  ��̾�����ɣ������ѹ�
      *  03.05.01    NACL-����    07/06/01  ���롼�׿����б�
      *
      *  04.07.01    NACL-ƣ��    13/01/21  ϫ�ҥ쥻���б�
      ************************************************************
      *01  RECE831-REC.
           03  RECE831-KEY.
               05  RECE831-KEY1.
      *���ŵ��أɣ�
                   07  RECE831-HOSPNUM     PIC 9(02).
      *����ǯ��
                   07  RECE831-SRYYM       PIC 9(06).
      *������ʬ
                   07  RECE831-NYUGAIKBN   PIC X(01).
      *���ԣɣ�
                   07  RECE831-PTID        PIC 9(10).
      *�쥻�ץȲ�
                   07  RECE831-RECEKA      PIC X(02).
      *�ݸ��ɣ�        
                   07  RECE831-HKNID       PIC 9(10).
      *Ŭ�ѳ���ǯ����
                   07  RECE831-TEKSTYMD    PIC X(08). 
      *����̾�ե饰
               05  RECE831-SYUBYOFLG       PIC X(01).
      *���ų�����
               05  RECE831-SRYYMD          PIC X(08).
      *ž����ʬ
               05  RECE831-TENKIKBN        PIC X(01).
      *ž����
               05  RECE831-TENKIYMD        PIC X(08).
      *Ϣ�ֹ�
               05  RECE831-RENNUM          PIC 9(03).
      *�����ֹ�
           03  RECE831-PTNUM               PIC X(20).
      *���Ų�
           03  RECE831-SRYKA               PIC X(02).
      *������̾������
           03  RECE831-KHNBYOMEICD         PIC X(07).
      *��̾������
           03  RECE831-BYOMEICD-G.
               05  RECE831-BYOMEICD        PIC X(07)
                                           OCCURS   21.
      *�����ե饰
           03  RECE831-UTAGAIFLG           PIC X(01).
      *�����ե饰
           03  RECE831-MANSEIKBN           PIC 9(02).
      *�ݸ��ȹ��ֹ�
           03  RECE831-HKNCOMBI            PIC X(04).
      *�쥻�ץ�ɽ���ե饰
           03  RECE831-REZEFLG             PIC X(01).
      *�쥻�ץ�ɽ�����
           03  RECE831-REZEMM              PIC 9(02).
      *��̾
           03  RECE831-BYOMEI              PIC X(160).
      *��̾ʸ����
           03  RECE831-BYOMEIMOJI          PIC 9(03).
      *��­������
           03  RECE831-HOSOKU-COMMENT      PIC X(40).
      *������̾
           03  RECE831-IN-BYOMEI           PIC X(160).
      *������̾ʸ����
           03  RECE831-IN-BYOMEIMOJI       PIC 9(03).
      *��̾�Խ��ե饰
           03  RECE831-BYOMEIHENFLG        PIC X(01).
      *�쥻�ץȼ���
           03  RECE831-RECESYUBETU         PIC 9(04).