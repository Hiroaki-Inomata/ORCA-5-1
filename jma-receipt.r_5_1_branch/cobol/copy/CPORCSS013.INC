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
      *    ��������å��ޥ������Խ����֡�COPY (CPORCSS013.INC)
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  SS013-AREA.
           03  SS013-FUNC                   PIC X(20).
      *
      *    ������ʬ
      *    ����:�����å��ޥ�����Ŭ����̾�ޥ���
      *    1:Ŭ����̾�ޥ����Τ�
      *    2:�����å��ޥ����Τ�
      *    9:�ǡ�����Ͽ�ʤ�
           03  SS013-CREATEKBN              PIC X(01).
           03  SS013-CHKKBN                 PIC X(01).
           03  SS013-SRYCD                  PIC X(09).
           03  SS013-KJNYMD                 PIC X(08).
      *    Ŭ����̾�ޥ�����ʬ
      *    0:Ŭ����̾�ޥ����򥤥�ݡ��Ȥ��ʤ�
      *    1:Ŭ����̾�ޥ����򥤥�ݡ��Ȥ���
      *    2:�����å��ޥ���������˽��ä�Ŭ����̾�ޥ����򥤥�ݡ��Ȥ���
           03  SS013-TEKBYOKBN              PIC X(01).
           03  SS013-TEMP-NAME              PIC X(10).
           03  SS013-TEMP-KEY               PIC X(16).