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
      *    �쥻�ץȼ����ֵѥѥ�᥿      COPY (CPORCSSYUBETUGET.INC)
      *                                  ���ѥץ������ (ORCSSYUBETUGET)
      *    �������
      *        ������ʬ
      *        ����ǯ��
      *        �ݸ��ֹ�
      *        ���ݶ�ʬ
      *        ���ݶ�ʬ
      *        Ϸ�Ͷ�ʬ
      *        �����
      *        �ܿͲ�²��ʬ
      *    �ֵѹ���
      *        �쥻�ץȼ���
      *        �쥻�ץȼ��̡������������
      *        �쥻�ץȼ��̡ʻ�쥻�ץȡ�
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.03.01    NACL-ƣ��    08/05/07  ���񣴼�ޤǤ�ʻ�Ѥε�Ͽ�б�
      *                                     �쥻�ץȼ��̡ʻ�쥻�ץȡˤ��ɲ�
      *
      *  04.07.01    NACL-ƣ��    14/03/30  ʿ������ǯ��������б�
      ************************************************************
      *
       01  ORCSSYUBETUGETAREA.
      *
           03  LNK-SYUBETUGET-SYOFLG           PIC 9(01).
      *
           03  LNK-SYUBETUGET-IN.
               05  LNK-SYUBETUGET-NYUGAIKBN    PIC X(01).
               05  LNK-SYUBETUGET-SRYYM        PIC 9(06).
               05  LNK-SYUBETUGET-HKNNUM       PIC X(03).
      *
               05  LNK-SYUBETUGET-KOKUHO       PIC X(01).
               05  LNK-SYUBETUGET-IHO          PIC X(01).
               05  LNK-SYUBETUGET-RJN          PIC X(01).
               05  LNK-SYUBETUGET-KOHI-CNT     PIC 9(01).
               05  LNK-SYUBETUGET-HONKZKKBN    PIC X(01).
      *
               05  LNK-SYUBETUGET-PREFNUM      PIC X(02).
               05  LNK-SYUBETUGET-HKN-HONKZKKBN
                                               PIC X(01).
               05  LNK-SYUBETUGET-HKN-HOJOKBN  PIC X(01).
               05  LNK-SYUBETUGET-HKN-HKNKOHSBTKBN
                                               PIC X(01).
      * 
               05  LNK-SYUBETUGET-HKNKBN       PIC X(01).
               05  LNK-SYUBETUGET-SAIGAIKBN    PIC X(01).
      *
               05  LNK-SYUBETUGET-KOHI-TBL     OCCURS  10.
                   07  LNK-SYUBETUGET-KOHID        PIC 9(10).
                   07  LNK-SYUBETUGET-KOHNUM       PIC X(03).
                   07  LNK-SYUBETUGET-KOHPAYKBN    PIC X(02).
                   07  LNK-SYUBETUGET-KOHHBTNUM    PIC X(02).
                   07  LNK-SYUBETUGET-RECESKYKBN   PIC X(01).
      *
               05  LNK-SYUBETUGET-AGE          PIC 9(03).
               05  LNK-SYUBETUGET-SYUGAKUKBN   PIC 9(01).
               05  LNK-SYUBETUGET-BIRTHDAY     PIC 9(08).
      *
               05  LNK-SYUBETUGET-RJNHKNNUM    PIC X(03).
               05  LNK-SYUBETUGET-RJNPAYKBN    PIC X(02).
      *
           03  LNK-SYUBETUGET-OT.
      *        �쥻�ץȼ��̡ʥ쥻�š�
               05  LNK-SYUBETUGET-RECESYUBETU  PIC 9(04).
      *        �쥻�ץȼ��̡������������
               05  LNK-SYUBETUGET-RECESYUBETU-X
                                               PIC 9(04).
      *        �쥻�ץȼ��̡ʻ�쥻�ץȡ�
               05  LNK-SYUBETUGET-RECESYUBETU-Y
                                               PIC 9(04).