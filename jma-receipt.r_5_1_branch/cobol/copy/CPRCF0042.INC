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
      *    �쥻�ץ����ٽ�     COPY (CPRCF0042.INC)               *
      *                       ���Ųʰ㤤��Ʊ��ޤΤޤȤ���       *  
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-����    04.02.05  ���ť����ɡ����̤��߷��ɲ�
      *  ��������ǯ��������б�
      *  01.01.01    NACL-����    04.04.10  ���𴵼�ˬ����ũ���ʹ�����
      *                                     Ƴ�����������ũ���ͤ��ɲ�
      *  01.01.02    NACL-����    04.08.17  �쥻�ץȲ��ɲ�
      *  01.01.03    NACL-����    05.07.05  ���ż��̶�ʬ�ɲ�
      *  01.01.04    NACL-����    05.11.11  �����ȥǡ����ɲ�
      *  02.09.01    NACL-����    05.04.17  ��������ǯ��������б�
      *
      *  03.05.01    NACL-ƣ��    07/04/19  ���롼�׿����б�
      *
      *  04.05.01    NACL-ƣ��    09/07/14  �������ʲ������б�
      *  04.05.02    NACL-����    12/04/10  �����б������ɲ�
      *
      *  04.06.01    NACL-ƣ��    10/10/12  ����ñ�ȤΥ쥻ʬΥ�б�
      *                                     ������kohid �ɲ�
      *  04.06.02    NACL-ƣ��    11/01/05  ��ǽ�ɲá����û���Ʊ��ʻ������
      *  04.06.03    NACL-ƣ��    12/10/26  �õ����ࣰ����Ϸʻ�˵ڤ�
      *                                     ������Ϸ��ˤΥ쥻�ץ��б�
      *  04.08.01    NACL-����    13/01/22  ��������ơ��֥룱�����б�
      *  04.08.02    NACL-����    13/01/25  ������ʬ�������ѥ쥻�ץȵ����б�
      *  04.08.03    NACL-����    16/02/05  ���ݥ쥻�ץȡ�¾�ݸ��ˤƿǻ�������
      *                                     �ѡץ������Ե����б�
      *  05.00.01    NACL-����    20/03/30  ���£�ǯ��������б�
      ************************************************************
      *01  RECE042-REC.
           03  RECE042-KEY.
               05  RECE042-KEY1.
      *���ŵ��أɣ�
                   07  RECE042-HOSPNUM     PIC 9(2).
      *����ǯ��
                   07  RECE042-SRYYM       PIC 9(06).
      *������ʬ
                   07  RECE042-NYUGAIKBN   PIC X(01).
      *���ԣɣ�
                   07  RECE042-PTID        PIC 9(10).
      *�쥻�ץȲ�
                   07  RECE042-RECEKA      PIC X(02).
      *������ʧ����
                   07  RECE042-TEISYUTUSAKI
                                           PIC 9(01).
      *�쥻�ץȼ���
                   07  RECE042-RECESYUBETU PIC 9(04).
      *�ݸ����ֹ�        
                   07  RECE042-HKNJANUM    PIC X(08).
      *�����ʬ
                   07  RECE042-HOJOKBN     PIC X(01).
      *����ɣ�
                   07  RECE042-KOHID       PIC 9(10).
      *Ŭ�ѳ���ǯ����
                   07  RECE042-TEKSTYMD    PIC X(08). 
      *ʬ���ʬ
                   07  RECE042-RECEIPT-KBN PIC X(10).
      *����ʬΥ��ʬ
               05  RECE042-KOHBUNRI        PIC  9(01).
      *���Ŷ�ʬ
               05  RECE042-SRYKBN          PIC  X(02).
      *���ż��̶�ʬ
               05  RECE042-SRYSYUKBN       PIC  X(03).
      *������
               05  RECE042-ZAITEN          PIC  9(08).
      *���ť����ɷ�
               05  RECE042-SRYCDTOTAL      PIC  9(14).
      *���̷�
               05  RECE042-SURYOUTOTAL     PIC  9(07)V9(05).
      *���ť����ɡ����̤��߷�
               05  RECE042-RUIKEITOTAL     PIC  9(13)V9(05).
      *���ٿ�
               05  RECE042-MEISAISU        PIC  9(04).
      *���𴵼�ˬ����ũ���ʹ�����Ƴ�����������ũ����
               05  RECE042-FLG-HOUTEN-COMMENT
                                           PIC  X(01).
      *¾�ݸ��ˤƿǻ���������Ե��ܥե饰
               05  RECE042-TAHOKENNGFLG    PIC  X(01).
      *���ݶ�ʬ
               05  RECE042-JIHOKBN         PIC  X(01).
      *�����Ⱥ�Ϣ��
               05  RECE042-COMRENNUM       PIC  9(05).
      *�޲��
           03  RECE042-ZAIKAISU            PIC  9(04).
      *��������ơ��֥�
           03  RECE042-DAY-AREA.
      *        ���Ų���ơ��֥��1:��ס�2-9:����������
               05  RECE042-DAY-TBL         OCCURS  10.
                   07  RECE042-KAISUINF    OCCURS  31.
                       09  RECE042-KAISU   PIC  9(03).
      *�ǡ���
           03  RECE042-REC1                PIC  X(2000).
      *�����ȥǡ���
           03  RECE042-COM-AREA.
               05  RECE042-COM             PIC  X(300)    OCCURS  10.
      *��ǡ�Ʊ��ʣ�����Ųʡ˻����
           03  RECE042-SYODOU-SRYKA-AREA.
               05  RECE042-SYODOU-SRYKA    PIC  X(02)     OCCURS  10.
      *�ƿǡ�Ʊ���������ܡ˻����
           03  RECE042-SAIDOU-SRYKA-AREA.
               05  RECE042-SAIDOU-SRYKA    PIC  X(02)     OCCURS  10.