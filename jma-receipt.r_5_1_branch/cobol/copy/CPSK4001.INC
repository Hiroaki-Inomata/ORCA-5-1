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
      *         �����ƥ�����ޥ���     (CPSK4001.INC)            *
      *         �ʣ���������                                     *
      *         ϫ�Ұ��ŵ��ؾ���                                 *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-ƣ��    02.09.17  �û�Ψ�ɲ�
      *  01.00.02    NACL-ƣ��    05.01.25  �����Ӳû����谷�ɲ�
      *  02.06.01    NACL-ƣ��    05/06/22  �����ղû�Ψ�ʣ���ˤ��ɲ�
      *  02.06.02    NACL-����    05/12/05  ϫ�ҥ쥻������ܤ��ɲ�
      ************************************************************
       01  SYS-4001-REC.
           03  SYS-4001-KEY.
               05  SYS-4001-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-4001-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-4001-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-4001-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-4001-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-4001-TBL.
      *ϫ�һ�����ŵ��ض�ʬ
               05  SYS-4001-RSIIRYOKBN  PIC  X(01).
      *������ŵ��إ�����
               05  SYS-4001-RSIIRYOCDN  PIC  X(80).
      *����ñ����ʬ
               05  SYS-4001-TENKBN      PIC  X(01).
      *�����ղû�Ψ
               05  SYS-4001-KSNRATE     PIC  9(02).
      *������ʸ����������
               05  SYS-4001-TAXKBN      PIC  X(01).
      *�����տ���������
               05  SYS-4001-SKJKBN      PIC  X(01).
      *��������������������
               05  SYS-4001-TAXKBN-KZI  PIC  X(01).
      *�����ո����Ӳû����谷
               05  SYS-4001-KOTEIKBN    PIC  X(01).
      *ϫ�ҡ���������񵭺ܶ�ʬ
               05  SYS-4001-RSIPRTKBN   PIC  X(01).
      *�����ղû�Ψ�ʣ����
               05  SYS-4001-KSNRATE1    PIC  9(03).
      *ϫ�ҥ쥻�������
               05  SYS-4001-ROUKAISU    PIC  X(01).
      *�����ն�ʬ
               05  SYS-4001-JIBAIKBN    PIC  X(01).
      *�����աʷ��ݽ���  ������ñ��    �㳰���
               05  SYS-4001-SYUTENTANKA-G.
                   07   SYS-4001-SYUTENTANKA-X PIC  X(02).
                   07   SYS-4001-SYUTENTANKA   REDEFINES
                        SYS-4001-SYUTENTANKA-X PIC  9(02).
      *�����աʷ��ݽ���  ��������ñ��  �㳰���
               05  SYS-4001-YKZTENTANKA-G.
                   07   SYS-4001-YKZTENTANKA-X PIC  X(02).
                   07   SYS-4001-YKZTENTANKA   REDEFINES
                        SYS-4001-YKZTENTANKA-X PIC  9(02).
      *��̳�ҳ���ʬ
               05  SYS-4001-KOMUKBN     PIC  X(01).
      *��̳�ҳ�ñ��
               05  SYS-4001-KOMUTENTANKA-G.
                   07   SYS-4001-KOMUTENTANKA-X PIC  X(02).
                   07   SYS-4001-KOMUTENTANKA   REDEFINES
                        SYS-4001-KOMUTENTANKA-X PIC  9(02).
      *�����������ʬ
               05  SYS-4001-JIBAI-SEIKYUKBN   PIC  X(01).
      *��̳�ҳ������ʬ
               05  SYS-4001-KOMU-SEIKYUKBN   PIC  X(01).
      *�ٶȾ������ܶ�ʬ
               05  SYS-4001-KYUGYO-PRTKBN   PIC  X(01).
      *��̳�ҳ�ϫ�ҽ������ñ����ʬ
               05  SYS-4001-KOMURSITANKAKBN PIC  X(01).
      *��̳�ҳ�ϫ�ҽ������ñ��
               05  SYS-4001-KOMURSITANKA-G.
                   07  SYS-4001-KOMURSITANKA    PIC  99V99.
      *�����աʷ��ݽ���  ������ñ��    ��������
               05  SYS-4001-SYUTENTANKA2-G.
                   07   SYS-4001-SYUTENTANKA2-X PIC  X(02).
                   07   SYS-4001-SYUTENTANKA2   REDEFINES
                        SYS-4001-SYUTENTANKA2-X PIC  9(02).
      *�����աʷ��ݽ���  ��������ñ��  ��������
               05  SYS-4001-YKZTENTANKA2-G.
                   07   SYS-4001-YKZTENTANKA2-X PIC  X(02).
                   07   SYS-4001-YKZTENTANKA2   REDEFINES
                        SYS-4001-YKZTENTANKA2-X PIC  9(02).
      *�軰�Թ԰ٸ�ʪ���ն�ʬ�������
               05  SYS-4001-DAISAN-GENBTKBN     PIC  X(01).
      *ͽ��
           03  SYS-4001-YOBI            PIC  X(386).
      *ü���ɣ�
           03  SYS-4001-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-4001-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-4001-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-4001-UPYMD           PIC  X(08).
      *��������
           03  SYS-4001-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-4001-UPD-KEY.
               05  SYS-4001-UPD-HOSPNUM     PIC 9(2).
               05  SYS-4001-UPD-KANRICD     PIC X(04).
               05  SYS-4001-UPD-KBNCD       PIC X(08).
               05  SYS-4001-UPD-STYUKYMD    PIC X(08).
               05  SYS-4001-UPD-EDYUKYMD    PIC X(08).
