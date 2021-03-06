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
      *    ����Ĵ������繹���ѥ�᥿����COPY (CPORCSCNYUHEN.INC)
      *                            ���ѥץ������ 
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCSCNYUHENAREA.
      *
      *����
           03  ORCSCNYUHEN-IN-AREA.
      *������ʬ
               05  ORCSCNYUHEN-KBN                 PIC  X(01).
      *        ��������ɬ�ܡ�
               05  ORCSCNYUHEN-SRYYMD              PIC  X(08).
      *������ʬ
               05  ORCSCNYUHEN-KBN2                PIC  X(01).
      *����
           03  ORCSCNYUHEN-OUT-AREA.
      *        �����������ꡢ���������ʤ�
               05  ORCSCNYUHEN-RRK-NYUIN           PIC 9(01).
      *        ���������桢�����������������
               05  ORCSCNYUHEN-NYUIN-KBN           PIC 9(01).
      *        ����������������
               05  ORCSCNYUHEN-NYUINYMD            PIC X(08).
               05  ORCSCNYUHEN-TAIINYMD            PIC X(08).
               05  ORCSCNYUHEN-NYUINKA             PIC X(02).
               05  ORCSCNYUHEN-HKNCOMBI            PIC X(04).
               05  ORCSCNYUHEN-DRCD                PIC X(05).
      *        ���ŷ��������
               05  ORCSCNYUHEN-NYUIN-DAYG.
                   07  ORCSCNYUHEN-NYUDAY          PIC 9(01)
                                                   OCCURS   31.
      *        �����������κǽ��ౡ��
               05  ORCSCNYUHEN-MAE-TAIINYMD        PIC X(08).
      *        �ǽ��ౡ��
               05  ORCSCNYUHEN-LAST-TAIINYMD       PIC X(08).
      *        ¾�屡�ౡ��
               05  ORCSCNYUHEN-TAI-TAIINYMD        PIC X(08).
      *        Ʊ�������̵ͭ
               05  ORCSCNYUHEN-DOUNYUKBN           PIC 9(01).
               05  ORCSCNYUHEN-DOUNYUIN            PIC 9(02).
      *        ��³���������
               05  ORCSCNYUHEN-STR-NYUINYMD        PIC X(08).
      *        Ʊ�������̵ͭ���ౡ����
               05  ORCSCNYUHEN-DOUTAIKBN           PIC 9(01).
      *
