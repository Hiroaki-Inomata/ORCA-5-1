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
      *    �������Ź԰ٹ����ѥ�᥿����COPY (CPORCSCNYUIN.INC)
      *                           ���ѥץ������ (ORCGK08,ORCSNYUACCT)
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSCNYUINAREA.
      *
      *��ʬ�ʣ������Ź԰ٺ������������Ź԰ٺ�����ౡ�ˡ�
           03  ORCSCNYUIN-KBN                        PIC  X(01).
      *�о����ա��ౡ����
           03  ORCSCNYUIN-YMD                        PIC  X(08).
      *�ౡ��
           03  ORCSCNYUIN-TAIINYMD                   PIC  X(08).
      *Ʊ����������ʬ
           03  ORCSCNYUIN-DOUJITSUKBN                PIC  X(01).
      *
