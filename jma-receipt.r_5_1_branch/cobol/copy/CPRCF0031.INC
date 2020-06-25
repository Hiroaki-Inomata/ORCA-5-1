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
      *    �쥻�ץ����ٽ񣰣��� COPY (CPRCF031.INC)     *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    MCC-ƣ��     01.06.29  �ݸ����ֹ桦
      *                                     Ŭ�ѳ���ǯ�������ɲ�
      *  01.00.02    MCC-ƣ��     01.09.28  ���ŵ��أɣĤν���
      *  01.00.03    MCC-ƣ��     01.12.17  ��̾�����ɤη���ν���
      *  01.00.04    MCC-ƣ��     02.04.19  ����̾�ե饰�ΰ����ѹ�
      *  01.00.05    MCC-ƣ��     02.06.11  ž������ˣţ٤��ɲ�
      *  01.00.06    NACL-ƣ��    03/03/12  �����ʬ���ɲ�
      *  01.00.07    NACL-ƣ��    04/02/01  ��̾�����ɣ������ѹ�
      *
      *  03.05.01    NACL-ƣ��    07/04/19  ���롼�׿����б�
      *
      *  04.05.01    NACL-ƣ��    09/11/25  �쥻�ť����ȳ�ĥ�б�
      *                                     ��­�����Ȥ��ɲ�
      *                                     Ϣ�ֹ�򣳷���ѹ�
      *                                     ������̾��������̾ʸ�������ɲ�
      *
      *  04.06.01    NACL-ƣ��    10/07/20  �쥻�ץ���̾���ܽ��б�
      *  04.06.02    NACL-ƣ��    10/10/12  ����ñ�ȤΥ쥻ʬΥ�б�
      *                                     ������kohid �ɲ�
      *  04.06.03    NACL-ƣ��    12/10/26  �õ����ࣰ����Ϸʻ�˵ڤ�
      *                                     ������Ϸ��ˤΥ쥻�ץ��б�
      ************************************************************
      *01  RECE031-REC.
           03  RECE031-KEY.
               05  RECE031-KEY1.
      *���ŵ��أɣ�
                   07  RECE031-HOSPNUM     PIC 9(2).
      *����ǯ��
                   07  RECE031-SRYYM       PIC 9(06).
      *������ʬ
                   07  RECE031-NYUGAIKBN   PIC X(01).
      *���ԣɣ�
                   07  RECE031-PTID        PIC 9(10).
      *�쥻�ץȲ�
                   07  RECE031-RECEKA      PIC X(02).
      *������ʧ����
                   07  RECE031-TEISYUTUSAKI
                                           PIC 9(01).
      *�쥻�ץȼ���
                   07  RECE031-RECESYUBETU PIC 9(04).
      *�ݸ����ֹ�        
                   07  RECE031-HKNJANUM    PIC X(08).
      *�����ʬ
                   07  RECE031-HOJOKBN     PIC X(01).
      *����ɣ�
                   07  RECE031-KOHID        PIC 9(10).
      *Ŭ�ѳ���ǯ����
                   07  RECE031-TEKSTYMD    PIC X(08). 
      *ʬ���ʬ
                   07  RECE01-RECEIPT-KBN  PIC X(10).
      *����̾�ե饰
               05  RECE031-SYUBYOFLG       PIC X(01).
      *���ų�����
               05  RECE031-SRYYMD          PIC X(08).
      *ž����ʬ
               05  RECE031-TENKIKBN-X      PIC X(01).
      *ž����
               05  RECE031-TENKIYMD-X      PIC X(08).
      *Ϣ�ֹ�
               05  RECE031-RENNUM          PIC 9(03).
      *�����ֹ�
           03  RECE031-PTNUM               PIC  X(20).
      *���Ų�
           03  RECE031-SRYKA               PIC  X(02).
      *ž����ʬ
           03  RECE031-TENKIKBN            PIC X(01).
      *ž����
           03  RECE031-TENKIYMD            PIC X(08).
      *������̾������
           03  RECE031-KHNBYOMEICD         PIC  X(07).
      *��̾������
           03  RECE031-BYOMEICD-G.
               05  RECE031-BYOMEICD-1    PIC X(07).
               05  RECE031-BYOMEICD-2    PIC X(07).
               05  RECE031-BYOMEICD-3    PIC X(07).
               05  RECE031-BYOMEICD-4    PIC X(07).
               05  RECE031-BYOMEICD-5    PIC X(07).
               05  RECE031-BYOMEICD-6    PIC X(07).
               05  RECE031-BYOMEICD-7    PIC X(07).
               05  RECE031-BYOMEICD-8    PIC X(07).
               05  RECE031-BYOMEICD-9    PIC X(07).
               05  RECE031-BYOMEICD-10   PIC X(07).
               05  RECE031-BYOMEICD-11   PIC X(07).
               05  RECE031-BYOMEICD-12   PIC X(07).
               05  RECE031-BYOMEICD-13   PIC X(07).
               05  RECE031-BYOMEICD-14   PIC X(07).
               05  RECE031-BYOMEICD-15   PIC X(07).
               05  RECE031-BYOMEICD-16   PIC X(07).
               05  RECE031-BYOMEICD-17   PIC X(07).
               05  RECE031-BYOMEICD-18   PIC X(07).
               05  RECE031-BYOMEICD-19   PIC X(07).
               05  RECE031-BYOMEICD-20   PIC X(07).
               05  RECE031-BYOMEICD-21   PIC X(07).
      *
           03  RECE031-BYOMEICD-R        REDEFINES   RECE031-BYOMEICD-G.
               05  RECE031-BYOMEICD      PIC X(07)
                                       OCCURS   21.
      *�����ե饰
           03  RECE031-UTAGAIFLG         PIC X(01).
      *�����ե饰
           03  RECE031-MANSEIKBN         PIC 9(02).
      *�ݸ��ȹ��ֹ�
           03  RECE031-HKNCOMBI          PIC X(04).
      *�쥻�ץ�ɽ���ե饰
           03  RECE031-REZEFLG           PIC X(01).
      *�쥻�ץ�ɽ�����
           03  RECE031-REZEMM            PIC 9(02).
      *��̾
           03  RECE031-BYOMEI            PIC X(160).
      *��̾ʸ����
           03  RECE031-BYOMEIMOJI        PIC 9(03).
      *��­������
           03  RECE031-HOSOKU-COMMENT    PIC X(40).
      *������̾
           03  RECE031-IN-BYOMEI         PIC X(160).
      *������̾ʸ����
           03  RECE031-IN-BYOMEIMOJI     PIC 9(03).
      *��̾�Խ��ե饰
           03  RECE031-BYOMEIHENFLG      PIC X(01).
      *