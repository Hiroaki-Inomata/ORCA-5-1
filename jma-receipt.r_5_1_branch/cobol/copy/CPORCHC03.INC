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
      *----------------------------------------------------------*
      *    �������μ�����ϥѥ�᥿      COPY (CPORCHC03.INC)
      *                                          (ORCHC03 �ǻ��ѡ�
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    MCC��¿��Ǽ  02/03/14  ��������ۤ��ɲ�
      *  01.00.02    NACL-¿��Ǽ  03/02/28  �ޤȤ���ɼ�ֹ���ɲ�
      *  01.00.03    NACL-¿��Ǽ  04/01/20  ȯ�ԥե饰¾�ɲ�
      *  01.00.04    NACL-¿��Ǽ  04/07/15  ʣ�����б�
      *  03.03.00    NACL-¿��Ǽ  06/09/27  �ֶ��б�
      *  03.05.01    NACL-¿��Ǽ  07/08/XX  ����������б�
      *  04.00.00    NACL-����    07/10/18  �ѻ極�����ɲ�
      ************************************************************
      *
       01  ORCHC03AREA.
      *
      *    ��ɼ�ֹ��ɬ�ܡ�
           03  ORCHC03-DENPNUM          PIC 9(07).
      *
      *    ���������
           03  ORCHC03-SEIKYU           PIC S9(07).
      *    ���������
           03  ORCHC03-NYUKIN           PIC S9(07).
      *    ��������ݾ����ǡʺƷǡ�
           03  ORCHC03-SEIKYU-TAX-SAI   PIC S9(07).
      *
      *****ʣ���ʤޤȤ���ɼ�ֹ�
      *    03  ORCHC03-FUKU-DENPNUM     PIC 9(07).
      *    �ޤȤ������
      *    03  ORCHC03-KON-SEIKYU       PIC S9(07).
      *    �ޤȤ�Ĵ����
      *****03  ORCHC03-KON-CHOSEI       PIC S9(07).
      *
      *    ȯ�ԥե饰�ʣ���������������ȯ�ԡ�������������������
           03  ORCHC03-HAKKOFLG         PIC  9(01).
      *    ����̤����
           03  ORCHC03-ZENMISYU         PIC S9(07).
      *    �����ֶ��
           03  ORCHC03-HENKIN           PIC S9(07).
      *
      *    ȯ����ˡ��1:�ʡ��ݸ��̡�2:�ݸ��̡�3:���̡�4:����)
      *             (0:��ɼ�ֹ�ʬ�Τ�)
           03  ORCHC03-HAKHOUFLG        PIC 9(01).
      *    ������������
           03  ORCHC03-OLD-AREA.
               05  ORCHC03-OLD-OCC      OCCURS   15.
      *            ��ɼ�ֹ�
                   07  ORCHC03-OLD-DENPNUM       PIC  9(07).
      *            ���Ų�
                   07  ORCHC03-OLD-SRYKA         PIC  X(02).
      *            �ݸ��ȹ礻
                   07  ORCHC03-OLD-HKNCOMBI      PIC  X(04).
      *            ������������
                   07  ORCHC03-OLD-SKYMONEY      PIC S9(07).
      *            �����������ǡʺƷǡ�
                   07  ORCHC03-OLD-SKYMONEY-SAI  PIC S9(07).
      *            ���������
                   07  ORCHC03-OLD-NYUKIN-TOTAL  PIC S9(07).
      *
      *            �����ֶ��
                   07  ORCHC03-KON-HENKIN        PIC S9(07).
      *    ���������ޤǤ��ֶ��
           03  ORCHC03-ZEN-HENKIN         PIC S9(07).
      *    ����̤����
           03  ORCHC03-ZENMISYU-2         PIC S9(07).
      *    ���������
           03  ORCHC03-KANYUKIN           PIC S9(07).
      *
      *    �������ɽ��
           03  ORCHC03-TEIKIN-KBN         PIC 9(01).
      *
      *    ��ɼ�ֹ���֡ʼ�Ǽ����κ�ȯ�ԡ�
           03  ORCHC03-DENPEDANUM      PIC  9(02).
      *    �ѥ�᥿�ơ��֥���ѥե饰
           03  ORCHC03-PARAFLG                 PIC 9(01).
      *    �ѥ�᥿�ơ��֥륭��
           03  ORCHC03-PARA-KEY.
      *        ��̳�ɣ�
               05  ORCHC03-PARA-GYOUMUID       PIC X(03).
      *        ü���ɣ�
               05  ORCHC03-PARA-TERMID         PIC X(64).
      *        �ե�����̾
               05  ORCHC03-PARA-FILEMEI        PIC X(20).
      *    �����ģ½��϶�ʬ(1:�����ģ¤˽��ϡ�
           03  ORCHC03-PRTKBN                  PIC X(01).
      *    ���ѥ�᥿
           03  ORCHC03-PRTAREA         PIC X(25000).
           03  ORCHC03-SIZE            PIC X(02).
      *
      *    ���긵��̳
      *    0:���ౡ��Ͽ��̳/���Ź԰ٶ�̳
      *    1:��Ǽ��̳�ʺ�ȯ�ԡ�
      *    2:��Ǽ��̳�ʷ��������ˡ�����Τ�
      *    3:��Ǽ��̳�������ǧ���̡�
      *    4:��Ǽ��̳�ʰ��������̡�
      *    5:����׶�̳�ʷ��������ˡ�����Τ�
      *    6:��������̳�������Τ�
      *    7:��������̳(�����ǧ����)�������Τ�
      *    8:�ౡ�����׻��������Τ�
           03  ORCHC03-MOTOID           PIC 9(02).
      *    �ޤȤ����ֶ��ʬ
      *    (ORCSNYUKIN���ֵ��ͤ򤽤Τޤ�����)
           03  ORCHC03-MATOME-NYUHENKIN-KBN.
               05  ORCHC03-MATOME-NYUKIN-KBN   PIC X(01).
               05  ORCHC03-MATOME-HENKIN-KBN   PIC X(01).