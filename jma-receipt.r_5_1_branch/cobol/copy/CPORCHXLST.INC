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
      *    �����å��ޥ������������ѥ�᥿��   COPY (CPORCHXLST.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *
       01  ORCHXLSTAREA.
      *
      *    �����å���ʬ
           03  HXLST-CHKKBN             PIC  X(01).
      *    �оݥ����ɡʳ��ϡ�
           03  HXLST-SRYCD              PIC X(09).
      *    �оݥ����ɡʽ�λ��
           03  HXLST-SRYCD-END          PIC X(09).
      *
      *    �оݣɣ�
           03  HXLST-ID                 PIC 9(09).
      *    ������ʬ
           03  HXLST-PROCKBN            PIC X(01).
      *    �����ȶ�ʬ
           03  HXLST-SRTKBN             PIC X(01).
      *    �����
           03  HXLST-KJNYMD             PIC X(08).
      *    Ŭ����̾�ޥ���ʬȽ�̥���
           03  HXLST-TEMP-NAME          PIC X(10).
           03  HXLST-TEMP-KEY           PIC X(16).
      *    Ŭ����̾�ޥ����оݶ�ʬ�ʥޥ�����
      *    0:���޵ڤӿ��Ź԰�
      *    1:����
      *    2:���Ź԰�
           03  HXLST-TEKBYOMST          PIC X(01).
      *    Ŭ����̾�ޥ����оݶ�ʬ���ϰϡ�
      *    1:�����å��ޥ�����Ͽʬ�Τ߰�������
      *    2:���������ɤȲ�����ο������Ƥ���������
      *    3:���ޤ��ϣ��˳��������Τ��������
      *    4:���ư�������
           03  HXLST-TEKBYOPRT          PIC X(01).
      *    ���ѥ�᥿
           03  HXLST-PRTAREA            PIC X(25000).
      *    ���Ϸ��
           03  HXLST-OUT-CNT            PIC 9(8).
      *    ����������
           03  HXLST-RCD                PIC S9(9)   BINARY.
      *