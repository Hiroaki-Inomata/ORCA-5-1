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
      *         �����ƥ�����ޥ���     (CPSK1001.INC)   *
      *         �ʣ���������                            *
      *         ���ŵ��ؾ���ݴ��ܾ���                  *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    MCC-¿��Ǽ   01.06.18  ������������ȯ�ԥե饰
      *                                     �������ɽ���ե饰�������ɲ�
      *  01.00.02    NACL-����    02.08.16  ���޾���ȯ�ԥե饰�ɲ�
      *  01.00.03    NACL-¿��Ǽ  03.02.10  �¾����ʰ��̡��ɲ�
      *  01.00.04    NACL-¿��Ǽ  05.10.14  �ǡ��������ե饰�ɲ�
      *  01.00.05    NACL-����    05.10.14  �����ݸ��������ʬ�ɲ�
      *  01.00.06    NACL-¿��Ǽ  06.04.07  ���������ٽ�ȯ�Զ�ʬ�ɲ�
      *  04.02.00    NACL-¿��Ǽ  08.03.07  ������Ģȯ�Զ�ʬ�ɲ�
      *  04.04.00    NACL-¿��Ǽ  09.01.23  ͽ��ɼȯ�Զ�ʬ�ɲ�
      *  04.04.00    NACL-��Ʀ��  09.01.26  �ܱ�ʬ���б�
      *                                     û�̰��ŵ���̾�Ρ��ɲ�       
      *  04.06.00    NACL-��ƣ    10.02.13  �ǡ��������ɶ�ʬ�ɲ�
      *  04.08.00    NACL-¿��Ǽ  17.09.05  ���񥳡���ü�������ɲ�
      ************************************************************
       01  SYS-1001-REC.
           03  SYS-1001-KEY.
               05  SYS-1001-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-1001-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-1001-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-1001-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-1001-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-1001-TBL.
      *���ֹ�
               05  SYS-1001-PREFNUM     PIC  9(02).
      *����ɽ��ʬ
               05  SYS-1001-TENHYOKBN   PIC  9(01).
      *���ŵ��إ�����
               05  SYS-1001-HOSPCD      PIC  9(07).
      *���ŵ��ؼ���
               05  SYS-1001-HOSPSBT     PIC  9(01).
      *���ŵ��أɣ�
               05  SYS-1001-HOSPID      PIC  X(24).
      *���ŵ���̾��
               05  SYS-1001-HOSPNAME    PIC  X(120).
      *���߼�̾
               05  SYS-1001-KAISETUNAME PIC  X(120).
      *������̾
               05  SYS-1001-KANRINAME   PIC  X(80).
      *�¾���
               05  SYS-1001-BEDSU       PIC  9(04).
      *Ϸ�ͻ�ʧ��ʬ
               05  SYS-1001-ROUPAYKBN   PIC  X(01).
      *�������±�
               05  SYS-1001-HOSPSBT1    PIC  9(01).
      *���ŵ��إ�����
               05  SYS-1001-HOSPCDN     PIC  X(40).
      *����������ʬ
               05  SYS-1001-SYOHOKBN    PIC  X(01).
      *�����ȯ�ԥե饰
               05  SYS-1001-SKYPRTFLG   PIC  X(01).
      *----(01.00.01) LINE ADD START ----------------------------------
      *������������ȯ�ԥե饰
               05  SYS-1001-SYOHOPRTFLG      PIC  X(01).
      *�������ɽ���ե饰
               05  SYS-1001-ZENSYOHOFLG      PIC  X(01).
      *----(01.00.01) LINE ADD END   ----------------------------------
      *----(01.00.02) LINE ADD START ----------------------------------
      *���޾���ȯ�ԥե饰
               05  SYS-1001-YAKUJYOPRTFLG    PIC  X(01).
      *�¾����ʰ��̡�
               05  SYS-1001-BEDSUIPN    PIC  9(04).
      *�����ü������ʸ���ͭ��
               05  SYS-1001-DISCOUNT-HASU-KBN
                                        PIC  X(01).
      *������ü������
               05  SYS-1001-TAX-HASU-KBN
                                        PIC  X(01).
      *�����ü��������ݡʸ���̵���ݸ�ʬ��
               05  SYS-1001-DISCOUNT-HASU-KBN2
                                        PIC  X(01).
      *�������������ٳ�����׻���ʬ
               05  SYS-1001-HIWARIKBN   PIC  X(01).
      *�ǡ��������ե饰
               05  SYS-1001-DATACREATEFLG    PIC  X(01).
      *�ǡ��������ˡ��ʬ
               05  SYS-1001-DATATEISYUTUKBN  PIC  X(01).
      *�����ݸ��������ʬ
               05  SYS-1001-JIHIPOSITION     PIC  X(01).
      *���������ٽ�ȯ�Զ�ʬ
               05  SYS-1001-MEIPRTFLG        PIC  X(01).
      *���������ݸ��ֹ楿�ֶ�ʬ
               05  SYS-1001-CHIHOUNUMKBN     PIC  X(01).
      *���ȷ׻��оݶ�ʬ
               05  SYS-1001-GENTAIKBN        PIC  X(01).
      *������Ģȯ�ԥե饰
               05  SYS-1001-OKUSURIPRTFLG    PIC  X(01).
      *ͽ��ɼȯ�ԥե饰
               05  SYS-1001-YYKHYOPRTFLG     PIC  X(01).
      *û�̰��ŵ���̾��
               05  SYS-1001-TANHOSPNAME      PIC  X(20).
      *�����ü��������ݡʸ���̵������ʬ��
               05  SYS-1001-DISCOUNT-HASU-KBN2J
                                        PIC  X(01).
      *�����ü������ϫ�ҡʸ���̵���ݸ�ʬ��
               05  SYS-1001-DISCOUNT-HASU-KBN3
                                        PIC  X(01).
      *�����ü������ϫ�ҡʸ���̵������ʬ��
               05  SYS-1001-DISCOUNT-HASU-KBN3J
                                        PIC  X(01).
      *�����ü�����꼫���աʸ���̵���ݸ�ʬ��
               05  SYS-1001-DISCOUNT-HASU-KBN4
                                        PIC  X(01).
      *�����ü�����꼫���աʸ���̵������ʬ��
               05  SYS-1001-DISCOUNT-HASU-KBN4J
                                        PIC  X(01).
      *�����ü����������ʸ���̵������ʬ��
               05  SYS-1001-DISCOUNT-HASU-KBN5J
                                        PIC  X(01).
      *ʬ�ڵ��ش����ֹ�
               05  SYS-1001-SANKANUM    PIC  9(10).
      *�ǡ��������ɶ�ʬ
               05  SYS-1001-DATAINFECTIONKBN
                                        PIC  X(01).
      *�����ü�������軰�Թ԰١ʸ���̵���ݸ�ʬ��
               05  SYS-1001-DISCOUNT-HASU-KBN6
                                        PIC  X(01).
      *�����ü�������軰�Թ԰١ʸ���̵������ʬ��
               05  SYS-1001-DISCOUNT-HASU-KBN6J
                                        PIC  X(01).
      *�軰�Թ԰١ʰ��������ô��۷׻���ʬ
               05  SYS-1001-DAISANKBN   PIC  X(01).
      *���񥳡���ü������
               05  SYS-1001-JIHI-HASU-KBN
                                        PIC  X(01).
      *ͽ��
      ***  03                           PIC  X(94).
      ***  03                           PIC  X(86).
      **** 03                           PIC  X(83).
      ***  03  SYS-1001-YOBI            PIC  X(42).
      ***  03  SYS-1001-YOBI            PIC  X(41).
      ***  03  SYS-1001-YOBI            PIC  X(38).
           03  SYS-1001-YOBI            PIC  X(37).
      *----(01.00.02) LINE ADD END   ----------------------------------
      *ü���ɣ�
           03  SYS-1001-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-1001-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-1001-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-1001-UPYMD           PIC  X(08).
      *��������
           03  SYS-1001-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-1001-UPD-KEY.
               05  SYS-1001-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1001-UPD-KANRICD     PIC X(04).
               05  SYS-1001-UPD-KBNCD       PIC X(08).
               05  SYS-1001-UPD-STYUKYMD    PIC X(08).
               05  SYS-1001-UPD-EDYUKYMD    PIC X(08).
