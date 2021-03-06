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
      *    �쥻�ץ����ٽ�     COPY (CPRCF0442.INC)               *
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
      *  03.05.01    NACL-����    07/05/28  ���롼�׿����б�
      *
      *  04.05.01    NACL-ƣ��    09/07/14  �������ʲ������б�
      *  04.05.02    NACL-����    12/04/10  �����б������ɲ�
      *
      *  04.07.01    NACL-����    13/01/21  ϫ�ҥ쥻���б�
      *  04.08.01    NACL-����    13/01/22  ��������ơ��֥룱�����б�
      *  04.08.02    NACL-����    13/01/25  ������ʬ�������ѥ쥻�ץȵ����б�
      *  05.00.01    NACL-����    20/03/30  ���£�ǯ��������б�
      ************************************************************
      *01  RECE442-REC.
           03  RECE442-KEY.
      *���ŵ��أɣ�
               05  RECE442-HOSPNUM         PIC  9(02).
      *����ǯ��
               05  RECE442-SRYYM           PIC  9(06).
      *������ʬ
               05  RECE442-NYUGAIKBN       PIC  X(01).
      *���ԣɣ�
               05  RECE442-PTID            PIC  9(10).
      *�쥻�ץȲ�
               05  RECE442-RECEKA          PIC  X(02).
      *�ݸ��ɣ�        
               05  RECE442-HKNID           PIC  9(10).
      *Ŭ�ѳ���ǯ����
               05  RECE442-TEKSTYMD        PIC  X(08). 
      *���Ŷ�ʬ
               05  RECE442-SRYKBN          PIC  X(02).
      *���ż��̶�ʬ
               05  RECE442-SRYSYUKBN       PIC  X(03).
      *������
               05  RECE442-ZAITEN          PIC  9(08).
      *���ť����ɷ�
               05  RECE442-SRYCDTOTAL      PIC  9(14).
      *���̷�
               05  RECE442-SURYOUTOTAL     PIC  9(07)V9(05).
      *���ť����ɡ����̤��߷�
               05  RECE442-RUIKEITOTAL     PIC  9(13)V9(05).
      *���ٿ�
               05  RECE442-MEISAISU        PIC  9(04).
      *���𴵼�ˬ����ũ���ʹ�����Ƴ�����������ũ����
               05  RECE442-FLG-HOUTEN-COMMENT
                                           PIC  X(01).
      *���ݶ�ʬ
               05  RECE442-JIHOKBN         PIC  X(01).
      *�����Ⱥ�Ϣ��
               05  RECE442-COMRENNUM       PIC  9(05).
      *�޲��
           03  RECE442-ZAIKAISU            PIC  9(04).
      *��������ơ��֥�
           03  RECE442-DAY-AREA.
      *        ���Ų���ơ��֥��1:��ס�2-9:����������
               05  RECE442-DAY-TBL         OCCURS  10.
                   07  RECE442-KAISUINF    OCCURS  31.
                       09  RECE442-KAISU   PIC  9(03).
      *�ǡ���
           03  RECE442-REC1                PIC  X(2000).
      *�����ȥǡ���
           03  RECE442-COM-AREA.
               05  RECE442-COM             PIC  X(300)    OCCURS  10.
      *��ǡ�Ʊ��ʣ�����Ųʡ˻����
           03  RECE442-SYODOU-SRYKA-AREA.
               05  RECE442-SYODOU-SRYKA    PIC  X(02)     OCCURS  10.
      *�ƿǡ�Ʊ���������ܡ˻����
           03  RECE442-SAIDOU-SRYKA-AREA.
               05  RECE442-SAIDOU-SRYKA    PIC  X(02)     OCCURS  10.
