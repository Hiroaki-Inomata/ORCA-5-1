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
      *    ������׺����ѥѥ�᥿������COPY (CPORCHCM34S01.INC)
      *                                ���ѥץ������ (ORCHCM34S01)
      *    �������
      *      ���ŵ��أɣ�
      *      ���ԣɣ�
      *      ����ǯ��
      *      �ݸ��ȹ礻
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  HCM34S01-AREA.
      *
      *���ŵ����ֹ�
           03  HCM34S01-HOSPNUM                PIC 9(2).
      *���ԣɣ�
           03  HCM34S01-PTID                   PIC 9(10).
      *����ǯ��
           03  HCM34S01-SANTEIYM               PIC X(06).
      *���ֻ����ʬ
           03  HCM34S01-KIKANKBN               PIC X(01).
      *������֡ʿ��������ٽ񤫤�θƤӽФ��κݤ����ꤹ���
           03  HCM34S01-SKYSTYMD               PIC X(08).
           03  HCM34S01-SKYEDYMD               PIC X(08).
      *���ᳫ����Ʊ���������ե饰
           03  HCM34S01-DOJITUFLG-ST           PIC  X(01).
      *����������������ʬ�ʣ������ʤ�������ī���顢�����뤫�顢����ͼ�����
           03  HCM34S01-TIMEKBN-ST             PIC  X(01).
      *���Ὢλ��Ʊ���������ե饰
           03  HCM34S01-DOJITUFLG-ED           PIC  X(01).
      *��λ������������ʬ�ʣ������ʤ�������ī�ޤǡ�������ޤǡ�����ͼ�ޤǡ�
           03  HCM34S01-TIMEKBN-ED             PIC  X(01).
      *�ݸ��ȹ礻
           03  HCM34S01-HKNCOMBI               PIC 9(04).
      *�ݸ��ֹ�
           03  HCM34S01-HKNNUM                 PIC X(03).
      *���Ųʶ�ʬ
           03  HCM34S01-SRYKAKBN               PIC X(01).
      *���Ų�
           03  HCM34S01-SRYKA                  PIC X(02).
      *���Ų�����
           03  HCM34S01-SRYKADAY-G.
               05  HCM34S01-SRYKADAY           PIC 9(01)
                                               OCCURS  31.
      *�����Խ��ѥ������ʬ
           03  HCM34S01-EXGAIHAKU              PIC X(01).
      *�����Խ��ѥ������ʬ
           03  HCM34S01-EXSYOKUJI              PIC X(01).
      *ɸ����ô���Խ���ʬ
           03  HCM34S01-SKJFTN                 PIC X(01).
      *�ֵѹ���
           03  HCM34S01-HENKYAKU-AREA.
      *
      *�������������Լ�����ô��
               05  HCM34S01-SENTEI-PTFTN       PIC 9(08).
      *�������۶�ʬ������
               05  HCM34S01-SAGAKUCD           PIC  X(02)
                                               OCCURS  31.
      *
      *�ֵѥơ��֥�
               05  HCM34S01-HENKYAKU-MAX       PIC 9(05).
               05  HCM34S01-HENKYAKU-TBL       OCCURS  100.
      *�������������Ŷ�ʬ
                   07  HCM34S01-SRYKBN         PIC  X(02).
      *���ż���
                   07  HCM34S01-SRYSYUKBN      PIC  X(03).
      *�޼��̶�ʬ�ʣ������������������񡡣����������ۡ���������
      *            �����ݸ��ȹ礻�������������û�����������ô��������
                   07  HCM34S01-ZAISKBKBN      PIC  X(01).
      *�¤�ͥ����
                   07  HCM34S01-PRIORITY-G.
                       09  HCM34S01-PRIORITY   PIC  9(03).
                       09  HCM34S01-PRIORITY2  PIC  9(03).
                       09  HCM34S01-PRIORITY3  PIC  9(03).
      *������
                   07  HCM34S01-ZAITEN         PIC  9(08).
      *�ޥ��ʥ��ե饰(���������ץ饹:0/�ޥ��ʥ�:1)
                   07  HCM34S01-ZAIMINUS       PIC  9(01).
      *�޲��
                   07  HCM34S01-ZAIKAISU       PIC  9(07).
      *���ٿ�
                   07  HCM34S01-MEISAISU       PIC  9(07).
      *���ť����ɹ��
                   07  HCM34S01-SRYCDTOTAL     PIC  9(14).
      *���̹��
                   07  HCM34S01-SURYOUTOTAL    PIC  9(07)V9(03).
      *�߷׹��
                   07  HCM34S01-RUIKEITOTAL    PIC  9(15)V9(03).
                   07  HCM34S01-SRYCD-OCC      OCCURS  20.
      *����
                       09  HCM34S01-SURYO      PIC  9(05).
      *���ť�����
                       09  HCM34S01-SRYCD      PIC  X(09).
      *���Ų���ơ��֥�
                   07  HCM34S01-DAY-TBL.
                       09  HCM34S01-DAY        PIC  9(03)
                                               OCCURS  31.
      *���������ɡʣ�������ǯ��顼���������Ը������顼�����������ʤ���
           03  HCM34S01-RC                     PIC S9(9) BINARY.
      *