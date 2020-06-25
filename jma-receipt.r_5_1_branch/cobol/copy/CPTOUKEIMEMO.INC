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
      *****************************************************************
      *  ���ץ��                 (CPTOUKEIMEMO.INC) 
      *****************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  TOUKEIMEMO-REC.
      *
      *����
           03  TKIMEMO-KEY.
               05  TKIMEMO-HOSPNUM           PIC 9(2).
      *�ץ������ɣ�
               05  TKIMEMO-PGID            PIC  X(20).
      *����������
               05  TKIMEMO-KANRICD         PIC  X(04).
      *��ʬ������
               05  TKIMEMO-KBNCD           PIC  X(08).
      *ͭ�����ֳ�����
               05  TKIMEMO-STYUKYMD        PIC  X(08).
      *ͭ�����ֽ�λ��
               05  TKIMEMO-EDYUKYMD        PIC  X(08).
      *���ϥѥ�᡼���ʣ�������
           03  TKIMEMO-PARA-G.
               05  TKIMEMO-PARA-ITEM.
                   07  TKIMEMO-PARA01          PIC  X(20).
                   07  TKIMEMO-PARA02          PIC  X(20).
                   07  TKIMEMO-PARA03          PIC  X(20).
                   07  TKIMEMO-PARA04          PIC  X(20).
                   07  TKIMEMO-PARA05          PIC  X(20).
                   07  TKIMEMO-PARA06          PIC  X(20).
                   07  TKIMEMO-PARA07          PIC  X(20).
                   07  TKIMEMO-PARA08          PIC  X(20).
               05  TKIMEMO-PARA-R      REDEFINES    TKIMEMO-PARA-ITEM.
                   07  TKIMEMO-PARA            PIC  X(20)
                                               OCCURS  8.
      *���
           03  TKIMEMO-MEMO                PIC  X(6000).
      *ü���ɣ�
           03  TKIMEMO-TERMID              PIC  X(32).
      *���ڡ��졼���ɣ�
           03  TKIMEMO-OPID                PIC  X(16).
      *����ǯ����
           03  TKIMEMO-CREYMD              PIC  X(08).
      *����ǯ����
           03  TKIMEMO-UPYMD               PIC  X(08).
      *��������
           03  TKIMEMO-UPHMS               PIC  X(06).
      *�ѥ�᥿��¸��ʬ
           03  TKIMEMO-SAVEPARA            PIC  X(01).
      *�ѥ�᥿��
           03  TKIMEMO-PARA09              PIC  X(20).
      *�ѥ�᥿����
           03  TKIMEMO-PARA10              PIC  X(20).
      *�桼�������
           03  TKIMEMO-USERMEMO            PIC  X(5000).
