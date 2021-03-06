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
      *    �����쥻�ץȡ���������         COPY (CPRCF121.INC) 
      *                       ���Ųʰ㤤��Ʊ��ޤΤޤȤ���         
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.05.01    NACL-ƣ��    09/07/14  �������ʲ������б�
      *  04.05.02    NACL-����    12/04/10  �����б������ɲ�
      *  04.07.01    NACL-����    13/01/22  �ǡ����ΰ��ĥ
      *  04.08.01    NACL-����    13/01/22  ��������ơ��֥룱�����б�
      *  04.08.02    NACL-����    13/01/25  ������ʬ�������ѥ쥻�ץȵ����б�
      *  05.00.01    NACL-����    20/03/30  ���£�ǯ��������б�
      ************************************************************
      *01  RECE121-REC.
           03  RECE121-KEY.
               05  RECE121-KEY1.
      *���ŵ����ֹ�
                   07  RECE121-HOSPNUM     PIC 9(2).
      *����ǯ��
                   07  RECE121-SRYYM       PIC 9(06).
      *������ʬ
                   07  RECE121-NYUGAIKBN   PIC X(01).
      *���ԣɣ�
                   07  RECE121-PTID        PIC 9(10).
      *�쥻�ץȿ��Ų�
                   07  RECE121-RECEKA      PIC X(02).
      *����ɣ�
                   07  RECE121-HKNID       PIC 9(10).
      *Ŭ�ѳ���ǯ����
                   07  RECE121-TEKSTYMD    PIC X(08). 
      *���Ŷ�ʬ
               05  RECE121-SRYKBN          PIC X(02).
      *���ż��̶�ʬ
               05  RECE121-SRYSYUKBN       PIC X(03).
      *������
               05  RECE121-ZAITEN          PIC 9(08).
      *���ť����ɷ�
               05  RECE121-SRYCDTOTAL      PIC 9(14).
      *���̷�
               05  RECE121-SURYOUTOTAL     PIC 9(07)V9(05).
      *���ť����ɡ����̤��߷�
               05  RECE121-RUIKEITOTAL     PIC 9(13)V9(05).
      *���ٿ�
               05  RECE121-MEISAISU        PIC 9(04).
      *���𴵼�ˬ����ũ���ʹ�����Ƴ�����������ũ����
               05  RECE121-FLG-HOUTEN-COMMENT
                                           PIC X(01).
      *���ݶ�ʬ
               05  RECE121-JIHOKBN         PIC X(01).
      *�����Ⱥ�Ϣ��
               05  RECE121-COMRENNUM       PIC 9(05).
      *�޲��
           03  RECE121-ZAIKAISU            PIC 9(04).
      *��������ơ��֥�
           03  RECE121-DAY-AREA.
      *        ���Ų���ơ��֥��1:��ס�2-9:����������
               05  RECE121-DAY-TBL         OCCURS  10.
                   07  RECE121-KAISUINF    OCCURS  31.
                       09  RECE121-KAISU   PIC 9(03).
      *�ǡ���
           03  RECE121-REC1                PIC X(2000).
      *�����ȥǡ���
           03  RECE121-COM-AREA.
               05  RECE121-COM             PIC X(300)    OCCURS  10.
      *��ǡ�Ʊ��ʣ�����Ųʡ˻����
           03  RECE121-SYODOU-SRYKA-AREA.
               05  RECE121-SYODOU-SRYKA    PIC X(02)     OCCURS  10.
      *�ƿǡ�Ʊ���������ܡ˻����
           03  RECE121-SAIDOU-SRYKA-AREA.
               05  RECE121-SAIDOU-SRYKA    PIC X(02)     OCCURS  10.
