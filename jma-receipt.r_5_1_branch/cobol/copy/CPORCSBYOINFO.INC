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
      *    ��̾�����ֵ��ѥѥ�᥿        COPY (CPORCSBYOINFO.INC)
      *                                  ���ѥץ������ (ORCSBYOINFO)
      *    �������
      *      �����ֹ�
      *      ����ǯ��
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.08.01    NACL-ƣ��    16/02/29  ��ʻ�ɤ˷��������ɲ��б�
      *                                     ��������ʬ����ʻ�ɶ�ʬ
      *  04.08.02    NACL-ƣ��    16/09/08  ���Ƥδ�����̾����μ����б�
      *  04.08.03    NACL-ƣ��    17/05/15  ��­�����ȥ������ֵ��б�
      *
      *  05.00.01    NACL-ƣ��    19/05/14  �ౡ�������ֵ��б�
      *  05.00.02    NACL-ƣ��    19/06/17  �軰�Թ԰��ֵ��б�
      ************************************************************
      *
       01  ORCSBYOINFOAREA.
      *    �����ֹ�
           03  SBYOINFO-PTNUM              PIC X(20).
      *    ���ԣɣ�
           03  SBYOINFO-PTID               PIC 9(10).
      *    ����ǯ��
           03  SBYOINFO-SRYYM              PIC 9(06).
      *
           03  SBYOINFO-SELECT-MODE        PIC X(03).
      *
      *    �����ΰ�
           03  SBYOINFO-OUT-AREA.
      *        ��̾���󥪡��С��ե饰
      ***      05  SBYOINFO-OVERFLOW       PIC X(01).
      *
               05  SBYOINFO-OUT-TBL        OCCURS  200.
      *        ���Ų�
                   07  SBYOINFO-SRYKA      PIC X(02).
      *        ���ų�����
                   07  SBYOINFO-SRYYMD     PIC X(08).
      *        ��̾���ϥ�����
                   07  SBYOINFO-BYOMEIINPUTCD-G.
                       09  SBYOINFO-BYOMEIINPUTCD
                                           PIC X(20)   OCCURS  6.
      *        ��̾������
                   07  SBYOINFO-BYOMEICD-G.
                       09  SBYOINFO-BYOMEICD
                                           PIC X(07)   OCCURS   21.
      *        �ե饰
                   07  SBYOINFO-FLG-G.
                       09  SBYOINFO-FLG    PIC X(01)   OCCURS   21.
      *        ��̾
                   07  SBYOINFO-BYOMEI-G.
                       09  SBYOINFO-BYOMEI-T
                                           PIC X(200)  OCCURS   21.
      *        ��̾
                   07  SBYOINFO-BYOMEI     PIC X(160).
      *        �������̾
                   07  SBYOINFO-CHARTBYOMEI
                                           PIC X(160).
      *        ��­������
                   07  SBYOINFO-HOSOKU-COMMENT
                                           PIC X(80).
      *        ��­�����ȥ�����
                   07  SBYOINFO-HOSOKUCD-G.
                       09    SBYOINFO-HOSOKUCD
                                           PIC X(20)   OCCURS   3.
                   07  SBYOINFO-HOSOKUCD-COMMENT-G.
                       09  SBYOINFO-HOSOKUCD-COMMENT
                                           PIC X(80)   OCCURS   3.
      *        ž����ʬ
                   07  SBYOINFO-TENKIKBN   PIC X(01).
      *        ž����
                   07  SBYOINFO-TENKIYMD   PIC X(08).
      *        �����ե饰
                   07  SBYOINFO-UTAGAIFLG  PIC X(01).
      *        ����̾�ե饰
                   07  SBYOINFO-SYUBYOFLG  PIC X(01).
      *        �����ե饰
                   07  SBYOINFO-MANSEIKBN  PIC 9(02).
      *        ������ʬ
                   07  SBYOINFO-NYUGAIKBN  PIC X(01).
      *        �ݸ��ȹ礻�ֹ�
                   07  SBYOINFO-HKNCOMBI   PIC 9(04).
      *        �쥻�ץ�ɽ���ե饰
                   07  SBYOINFO-REZEFLG    PIC X(01).
      *        �쥻�ץ�ɽ�����
                   07  SBYOINFO-REZEMM     PIC 9(02).
      *        �ݸ���̾�ե饰
                   07  SBYOINFO-HKNBYOMEIFLG
                                           PIC X(01).
      *        ��̾�Խ��ե饰
                   07  SBYOINFO-BYOMEIHENFLG
                                           PIC X(01).
      *        ��������ʬ
                   07  SBYOINFO-CLASS1     PIC 9(02).
      *        ��ʻ�ɶ�ʬ
                   07  SBYOINFO-CLASS2     PIC 9(02).
      *        �ౡ������
                   07  SBYOINFO-DISCHARGEFLG
                                           PIC X(1).
      *        ����ɣġ��軰�Թ԰٤Τߡ�
                   07  SBYOINFO-KOHID      PIC 9(10).
      *   �꥿���󥳡���
           03  SBYOINFO-RC                 PIC 9(01).