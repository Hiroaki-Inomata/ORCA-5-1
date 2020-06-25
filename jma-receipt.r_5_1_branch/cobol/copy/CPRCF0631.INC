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
      *    �쥻�ץ����ٽ�������� ��̾���ؤ� (CPRCF063.INC)      *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-ƣ��    04/02/01  ��̾�����ɣ������ѹ�
      *  03.05.01    NACL-��Ʀ��  07/05/30  ���롼�׿����б�
      *
      *  04.05.01    NACL-ƣ��    09/11/25  �쥻�ť����ȳ�ĥ�б�
      *                                     ��­�����Ȥ��ɲ�
      *                                     Ϣ�ֹ�򣳷���ѹ�
      *                                     ������̾��������̾ʸ�������ɲ�
      *
      *  04.06.01    NACL-ƣ��    10/07/20  �쥻�ץ���̾���ܽ��б�
      *  04.06.02    NACL-ƣ��    10/10/12  ����ñ�ȤΥ쥻ʬΥ�б�
      *                                     ������kohid �ɲ�
      *
      *  04.07.01    NACL-ƣ��    12/12/17  ʣ����쥻�ץ�ʬ���б�
      ************************************************************
      *01  RECE631-REC.
           03  RECE631-KEY.
               05  RECE631-KEY1.
      *���ŵ��أɣ�
                   07  RECE631-HOSPNUM     PIC 9(2).
      *����ǯ��
                   07  RECE631-SRYYM       PIC 9(06).
      *������ʬ
                   07  RECE631-NYUGAIKBN   PIC X(01).
      *���ԣɣ�
                   07  RECE631-PTID        PIC 9(10).
      *�쥻�ץȲ�
                   07  RECE631-RECEKA      PIC X(02).
      *������ʧ����
                   07  RECE631-TEISYUTUSAKI
                                           PIC 9(01).
      *�쥻�ץȼ���
                   07  RECE631-RECESYUBETU PIC 9(04).
      *�ݸ����ֹ�        
                   07  RECE631-HKNJANUM    PIC X(08).
      *�����ʬ
                   07  RECE631-HOJOKBN     PIC X(01).
      *����ɣ�
                   07  RECE631-KOHID       PIC 9(10).
      *Ŭ�ѳ���ǯ����
                   07  RECE631-TEKSTYMD    PIC X(08). 
      *ʬ���ʬ
                   07  RECE631-RECEIPT-KBN PIC X(10).
      *����̾�ե饰
               05  RECE631-SYUBYOFLG       PIC X(01).
      *���ų�����
               05  RECE631-SRYYMD          PIC X(08).
      *ž����ʬ
               05  RECE631-TENKIKBN-X      PIC X(01).
      *ž����
               05  RECE631-TENKIYMD-X      PIC X(08).
      *Ϣ�ֹ�
               05  RECE631-RENNUM          PIC 9(03).
      *�����ֹ�
           03  RECE631-PTNUM               PIC  X(20).
      *���Ų�
           03  RECE631-SRYKA               PIC  X(02).
      *ž����ʬ
           03  RECE631-TENKIKBN            PIC X(01).
      *ž����
           03  RECE631-TENKIYMD            PIC X(08).
      *������̾������
           03  RECE631-KHNBYOMEICD         PIC  X(07).
      *��̾������
           03  RECE631-BYOMEICD-G.
               05  RECE631-BYOMEICD-1    PIC X(07).
               05  RECE631-BYOMEICD-2    PIC X(07).
               05  RECE631-BYOMEICD-3    PIC X(07).
               05  RECE631-BYOMEICD-4    PIC X(07).
               05  RECE631-BYOMEICD-5    PIC X(07).
               05  RECE631-BYOMEICD-6    PIC X(07).
               05  RECE631-BYOMEICD-7    PIC X(07).
               05  RECE631-BYOMEICD-8    PIC X(07).
               05  RECE631-BYOMEICD-9    PIC X(07).
               05  RECE631-BYOMEICD-10   PIC X(07).
               05  RECE631-BYOMEICD-11   PIC X(07).
               05  RECE631-BYOMEICD-12   PIC X(07).
               05  RECE631-BYOMEICD-13   PIC X(07).
               05  RECE631-BYOMEICD-14   PIC X(07).
               05  RECE631-BYOMEICD-15   PIC X(07).
               05  RECE631-BYOMEICD-16   PIC X(07).
               05  RECE631-BYOMEICD-17   PIC X(07).
               05  RECE631-BYOMEICD-18   PIC X(07).
               05  RECE631-BYOMEICD-19   PIC X(07).
               05  RECE631-BYOMEICD-20   PIC X(07).
               05  RECE631-BYOMEICD-21   PIC X(07).
      *
           03  RECE631-BYOMEICD-R        REDEFINES   RECE631-BYOMEICD-G.
               05  RECE631-BYOMEICD      PIC X(07)
                                       OCCURS   21.
      *�����ե饰
           03  RECE631-UTAGAIFLG         PIC X(01).
      *�����ե饰
           03  RECE631-MANSEIKBN         PIC 9(02).
      *�ݸ��ȹ��ֹ�
           03  RECE631-HKNCOMBI          PIC X(04).
      *�쥻�ץ�ɽ���ե饰
           03  RECE631-REZEFLG           PIC X(01).
      *�쥻�ץ�ɽ�����
           03  RECE631-REZEMM            PIC 9(02).
      *��̾
           03  RECE631-BYOMEI            PIC X(160).
      *��̾ʸ����
           03  RECE631-BYOMEIMOJI        PIC 9(03).
      *��­������
           03  RECE631-HOSOKU-COMMENT    PIC X(40).
      *������̾
           03  RECE631-IN-BYOMEI         PIC X(160).
      *������̾ʸ����
           03  RECE631-IN-BYOMEIMOJI     PIC 9(03).
      *��̾�Խ��ե饰
           03  RECE631-BYOMEIHENFLG      PIC X(01).