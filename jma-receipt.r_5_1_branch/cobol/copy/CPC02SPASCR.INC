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
      *
      *    ��̾��Ͽ ���̣ӣУ� 
      *            ���̡��ã����ǻ��� 
      *          
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    MCC-ƣ��     01/06/06  �ݸ���̾�ե饰���ɲ�
      *  01.02.01    MCC-ƣ��     01/08/27  ���̽����ˤ����ܤν���            
      *  01.03.01    MCC-ƣ��     01/12/06  ������̾�ޥ������ɲ�     
      *
      *  01.01.01    NACL-ƣ��    03/08/28  ��Ͽ�������¤�ʤ���
      *  01.01.01    NACL-ƣ��    04/02/24  ������ʬ���ɲ�
      *
      *  02.06.01    NACL-ƣ��    05/08/26  �ݸ��ȹ礻���ɲ�
      *****************************************************************
      *
      *01  SPA-SCR-REC.
        02  SPA-SCR-C02-REC.
           03  SPA-GMN-C02-TBL.
               05  SPA-GMN-C02-TBLO            OCCURS   200.
                   07  SPA-GMN-C02-TBANGO      PIC 9(05).
                   07  SPA-GMN-C02-TSRYKA      PIC X(04).
                   07  SPA-GMN-C02-TSRYYMD     PIC X(09).
                   07  SPA-GMN-C02-TBYOMEI     PIC X(80).
                   07  SPA-GMN-C02-TUTAGAIFLG  PIC X(14).
                   07  SPA-GMN-C02-TSYUBYOFLG  PIC X(12).
                   07  SPA-GMN-C02-TMANSEIKBN  PIC X(42).
                   07  SPA-GMN-C02-TNYUGAIKBN  PIC X(06).
                   07  SPA-GMN-C02-THKNCOMBI   PIC X(40).
                   07  SPA-GMN-C02-TREZEFLG    PIC X(10).
                   07  SPA-GMN-C02-TREZEMM     PIC 9(02).
                   07  SPA-GMN-C02-TTENKIKBN   PIC X(04).
                   07  SPA-GMN-C02-TTENKIYMD   PIC X(09).
                   07  SPA-GMN-C02-THKNBYOMEIFLG
                                               PIC X(08).
      * 
           03  SPA-NAI-C02-TBL.
               05  SPA-NAI-C02-TBLO            OCCURS   200.
                   07  SPA-NAI-C02-TSRYYMD     PIC X(08).
                   07  SPA-NAI-C02-TRENNUM     PIC 9(02).
                   07  SPA-NAI-C02-TSRYKA      PIC X(02).
                   07  SPA-NAI-C02-TNYUGAIKBN  PIC X(01).
                   07  SPA-NAI-C02-TTENKIYMD   PIC X(08).
                   07  SPA-NAI-C02-THKNCOMBI   PIC X(04).