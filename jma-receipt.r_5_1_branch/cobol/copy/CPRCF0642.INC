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
      *    �쥻�ץ����ٽ�     COPY (CPRCF0642.INC)               *
      *                       ���Ųʰ㤤��Ʊ��ޤΤޤȤ���       *  
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-����    04.02.05  ���ť����ɡ����̤��߷��ɲ�
      *  01.00.02    NACL-����    04.08.17  �쥻�ץȲ��ɲ�
      *  01.00.03    NACL-����    05.07.05  ���ż��̶�ʬ�ɲ�
      *  01.00.04    NACL-����    05.11.11  �����ȥǡ����ɲ�
      *  02.09.01    NACL-����    05.04.17  ��������ǯ��������б�
      *  03.05.01    NACL-��Ʀ��  07/05/30  ���롼�׿����б�
      *
      *  04.05.01    NACL-ƣ��    09/07/14  �������ʲ������б�
      *
      *  04.06.01    NACL-ƣ��    10/10/12  ����ñ�ȤΥ쥻ʬΥ�б�
      *                                     ������kohid �ɲ�
      *  04.06.02    NACL-ƣ��    11/01/05  ��ǽ�ɲá����û���Ʊ��ʻ������
      *
      *  04.07.01    NACL-ƣ��    12/12/17  ʣ����쥻�ץ�ʬ���б�
      *  04.07.02    NACL-����    15/08/10  �����ݸ��ȹ礻�ֹ��ɲ�
      *
      *  04.08.01    NACL-����    13/01/25  ������ʬ�������ѥ쥻�ץȵ����б�
      *  04.08.02    NACL-����    16/08/23  �������б�
      *  05.00.01    NACL-����    20/03/30  ���£�ǯ��������б�
      ************************************************************
      *01  RECE642-REC.
           03  RECE642-KEY.
               05  RECE642-KEY1.
      *���ŵ��أɣ�
                   07  RECE642-HOSPNUM     PIC 9(2).
      *����ǯ��
                   07  RECE642-SRYYM       PIC 9(06).
      *������ʬ
                   07  RECE642-NYUGAIKBN   PIC X(01).
      *���ԣɣ�
                   07  RECE642-PTID        PIC 9(10).
      *�쥻�ץȲ�
                   07  RECE642-RECEKA      PIC X(02).
      *������ʧ����
                   07  RECE642-TEISYUTUSAKI
                                           PIC 9(01).
      *�쥻�ץȼ���
                   07  RECE642-RECESYUBETU PIC 9(04).
      *�ݸ����ֹ�        
                   07  RECE642-HKNJANUM    PIC X(08).
      *�����ʬ
                   07  RECE642-HOJOKBN     PIC X(01).
      *����ɣ�
                   07  RECE642-KOHID       PIC 9(10).
      *Ŭ�ѳ���ǯ����
                   07  RECE642-TEKSTYMD    PIC X(08). 
      *ʬ���ʬ
                   07  RECE642-RECEIPT-KBN PIC X(10).
      *����ʬΥ��ʬ
               05  RECE642-KOHBUNRI        PIC  9(01).
      *���Ŷ�ʬ
               05  RECE642-SRYKBN          PIC  X(02).
      *���ż��̶�ʬ
               05  RECE642-SRYSYUKBN       PIC  X(03).
      *������
               05  RECE642-ZAITEN          PIC  9(08).
      *���ť����ɷ�
               05  RECE642-SRYCDTOTAL      PIC  9(14).
      *���̷�
               05  RECE642-SURYOUTOTAL     PIC  9(07)V9(05).
      *���ť����ɡ����̤��߷�
               05  RECE642-RUIKEITOTAL     PIC  9(13)V9(05).
      *���ٿ�
               05  RECE642-MEISAISU        PIC  9(04).
      *���ݶ�ʬ
               05  RECE642-JIHOKBN         PIC  X(01).
      *��������ʬ
               05  RECE642-JISANYAKKBN     PIC  X(01).
      *�����Ⱥ�Ϣ��
               05  RECE642-COMRENNUM       PIC  9(05).
      *�޲��
           03  RECE642-ZAIKAISU            PIC  9(04).
      *��������ơ��֥�
           03  RECE642-DAY-AREA.
      *        ���Ų���ơ��֥��1:���,2:����,3:����, 4:����ʾ��
               05  RECE642-DAY-TBL         OCCURS  4.
                   07  RECE642-KAISUINF    OCCURS  31.
                       09  RECE642-KAISU   PIC  9(03).
      *�����ݸ��ȹ礻�ֹ�
           03  RECE642-HKNCOMBI-DAY-AREA.
               05  RECE642-HKNCOMBI-DAY    PIC  9(04)     OCCURS  31.
      *�ǡ���
           03  RECE642-REC1                PIC  X(2000).
      *�����ȥǡ���
           03  RECE642-COM-AREA.
               05  RECE642-COM             PIC  X(300)    OCCURS  10.
      *��ǡ�Ʊ��ʣ�����Ųʡ˻����
           03  RECE642-SYODOU-SRYKA-AREA.
               05  RECE642-SYODOU-SRYKA    PIC  X(02)     OCCURS  10.
