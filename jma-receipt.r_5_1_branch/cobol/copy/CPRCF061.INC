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
      *    �쥻�ץ����ٽ񡦰����������������� COPY (CPRCF061.INC)      *
      ********************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-ƣ��    03.03.03  ���Ųʿ��������飲�����ѹ�
      *  01.00.02    NACL-ƣ��    03.03.11  �쥻�ץȼ�������������ɲ�
      *  01.00.03    NACL-ƣ��    03.07.28  ���Ų���ơ��֥���ɲ�
      *  01.00.04    NACL-ƣ��    04.06.15  ��������ɲáʸ��̺������Τߡ�
      *  01.00.05    NACL-ƣ��    04.10.31  ���Ծ��󡦸��ֹ���ɲ�
      *  01.00.06    NACL-ƣ��    04.11.01  ����쥻�ץ������ʬ���ɲ�
      *
      *  ver.2.6.0 
      *  02.06.01    NACL-ƣ��    05.07.13  �ݸ��ȹ礻�����ݸ�������ɲ� 
      *  02.06.02    NACL-ƣ��    05.07.28  �쥻�ץ����ᤷ�ʤ�������ɲ�
      *
      *  03.04.01    NACL-ƣ��    06/12/05  ̤����쥻�б�
      *
      *  03.05.01    NACL-ƣ��    07/04/19  ���롼�׿����б�
      * 
      *  04.02.01    NACL-ƣ��    08/03/04  ʿ������ǯ��������б�
      *                                     ��������ʬ���ɲ�
      *
      *  04.05.01    NACL-ƣ��    11/03/02  ���縩�б�
      *                                     ������������������������ѹ�
      *                                     �ˤʤä����ε��ܽ�
      *                                     ����Ŭ�ѳ��������ɲ�
      *  04.05.02    NACL-ƣ��    11/03/25  �ҳ��б�
      *                                     Ϣ��������ɲ�
      *  04.05.03    NACL-ƣ��    11/11/22  Ʊ���������б�
      *
      *  04.06.01    NACL-ƣ��    10/10/12  ����ñ�ȤΥ쥻ʬΥ�б�
      *                                     ������kohid �ɲ�
      *  04.06.02    NACL-ƣ��    11/12/12  �����쥻������������б�
      *  04.06.03    NACL-ƣ��    12/10/04  Ʊ��쥻�ץȤθ��񤬣��Ĥ�Ķ
      *                                     �������μ��������б�
      *  04.06.04    NACL-ƣ��    12/10/26  �õ����ࣰ����Ϸʻ�˵ڤ�
      *                                     ������Ϸ��ˤΥ쥻�ץ��б�
      *  04.06.05    NACL-ƣ��    12/12/21  ����������ǻ��ꤷ������
      *                                     ���ౡ�򵭺��б�
      *
      *  04.07.01    NACL-ƣ��    14/12/08  ������˸������ô���ֹ�����
      *                                     �ѹ������ä����ε����б�
      *                                     ˡ���ֹ桢Ŭ�ѳ������ν�Ȥ���
      *                                     ����������� 
      *                                     ˡ���ֹ��ɲ�
      *  04.07.02    NACL-ƣ��    14/11/13  ���ݤ���ô���ֹ桢������ֹ�
      *                                     ��Ʊ���Ȥ��ϤޤȤ��
      *
      *  05.00.01    NACL-ƣ��    19/04/04  ������η���ѹ�
      ********************************************************************
      *01  RECE61-REC.
           03  RECE61-KEY.
      *���ŵ��أɣ�
               05  RECE61-HOSPNUM         PIC 9(2).
      *����ǯ��
               05  RECE61-SRYYM           PIC  9(06).
      *������ʬ
               05  RECE61-NYUGAIKBN       PIC  X(01).
      *���ԣɣ�
               05  RECE61-PTID            PIC  9(10).
      *ʬ���ʬ
               05  RECE61-RECEIPT-KBN     PIC  X(10).
      *�쥻�ץȿ��Ų�
               05  RECE61-RECEKA          PIC  X(02).
      *���ݸ��ݸ��ֹ�
               05  RECE61-HKNNUM          PIC  X(03).
      *����ɣġ�Ϸ���ݸ���
               05  RECE61-KOHID           PIC  9(10).
      *�ݸ����ֹ�        
               05  RECE61-HKNJANUM        PIC  X(08).
      *�ܿͲ�²��ʬ
               05  RECE61-HONKZKKBN       PIC  X(01).
      *�����ʬ
               05  RECE61-HOJOKBN         PIC  X(01).
      *����ɣġʸ���ñ�Ȼ����ѡ�
               05  RECE61-KOH-KOHID       PIC  9(10).
      *Ŭ�ѳ���ǯ����
               05  RECE61-TEKSTYMD        PIC  X(08). 
      *�����ΰ�
           03 RECE61-KOHI-AREA.
               05  RECE61-KOHI-TBL        OCCURS  10.
      *    ����ɣ�
                   07  RECE61-KOHID-O     PIC  9(10).
      *    �����ݸ��ֹ�
                   07  RECE61-KOHHKNNUM-O PIC  X(03).
      *    �����ʧ��ʬ
                   07  RECE61-KOHPAYKBN-O PIC  X(02).
      *    ����쥻�ץ������ʬ�ʶ��򡧥쥻���ܤΤߡ��������ܤʤ���
                   07  RECE61-RECESKYKBN-O
                                          PIC  X(01).
      *    ����Ŭ�ѳ���ǯ����
                   07  RECE61-KOHTEKSTYMD-O
                                          PIC  X(08).
      *    ����ˡ���ֹ�
                   07  RECE61-KOHHBTNUM-O PIC  X(02).
      *    ������ô���ֹ�
                   07  RECE61-KOHFTNJANUM-O
                                          PIC  X(08).
      *    ���������ֹ�
                   07  RECE61-KOHJKYSNUM-O
                                          PIC  X(20).
      *
      *���ݸ�����
           03  RECE61-HKN.
      *���ݸ����ݸ��ɣ�
               05  RECE61-HKNID           PIC  9(10).
      *���ݸ���ˡ���ֹ�
               05  RECE61-HKNHBTNUM       PIC  X(02).
      *���ݸ����ݸ����ֹ�
               05  RECE61-HKN-HKNJANUM    PIC  X(08).
      *���ݸ����ܿͲ�²��ʬ
               05  RECE61-HKN-HONKZKKBN   PIC  X(01).
      *���ݸ��������ʬ
               05  RECE61-HKN-HOJOKBN     PIC  X(01).
      *���ݸ��ݷ�³��ʬ
               05  RECE61-CONTKBN         PIC  X(01).
      *���ݸ��ݵ���
               05  RECE61-KIGO            PIC  X(80).
      *���ݸ����ֹ�
               05  RECE61-NUM             PIC  X(80).
      *���ݸ��ݾ������ֹ�
               05  RECE61-CERTNUM         PIC  X(03).
      *
      *�ݸ��ȹ礻�ֹ��ΰ�
           03 RECE61-HKNCOMBI-AREA.
               05  RECE61-HKNCOMBI-TBL    OCCURS  10.
      *�ݸ��ȹ礻�ֹ�
                   07  RECE61-HKNCOMBI-O  PIC  9(04).
      *���ݸ��ֹ�
                   07  RECE61-HKNNUMHC    PIC  X(03).
      *�ȹ礻�����ݸ��ֹ����
                   07  RECE61-KOHNUMHCINF OCCURS  4.
      *�ȹ礻�����ݸ��ֹ�
                       09  RECE61-KOHNUMHC
                                          PIC  X(03).
      *�ȹ礻�����ݸ��ɣ�
                       09  RECE61-KOHIDHC PIC  9(10).
      *
      *���Ųʾ���
           03 RECE61-SRYKAINF.
               05  RECE61-SRYKA           PIC  X(02)  OCCURS  20.
      *Ŭ��ǯ�����ΰ�
           03 RECE61-TEKYMD-AREA.
               05  RECE61-TEKYMD-TBL      OCCURS  10.
      *Ŭ�ѳ���ǯ����
                   07  RECE61-TEKSTYMDX   PIC  X(08). 
      *Ŭ�ѽ�λǯ����
                   07  RECE61-TEKEDYMDX   PIC  X(08). 
      *
      *�쥻�ץ����ᤷ�ʤ������ΰ�
           03 RECE61-KOHSKY-KOHI-AREA.
               05  RECE61-KOHSKY-KOHI-TBL OCCURS  4.
      *    ����ɣ�
                   07  RECE61-KOHSKY-KOHID-O
                                          PIC  9(10).
      *    �����ݸ��ֹ�
                   07  RECE61-KOHSKY-KOHHKNNUM-O
                                          PIC  X(03).
      *���Ų���ơ��֥�
           03  RECE61-DAY-AREA.
               05  RECE61-DAY             PIC  9(03)
                                               OCCURS   31.
      *���Ų���ơ��֥���������������ꤷ�Ƥ��ʤ��Ȥ���
           03  RECE61-DAY-1-AREA.
               05  RECE61-DAY-1           PIC  9(03)
                                               OCCURS   31.
      *���Ծ���
           03  RECE61-PTINF.
      *�����ֹ�  
               05  RECE61-PTNUM           PIC  X(20).
      *���ʻ�̾
               05  RECE61-KANANAME        PIC  X(50).
      *������̾
               05  RECE61-NAME            PIC  X(100).
      *����
               05  RECE61-SEX             PIC  X(01).
      *��ǯ����
               05  RECE61-BIRTHDAY        PIC  9(08).
      *��˴��ʬ
               05  RECE61-DEATHKBN        PIC  X(01).
      *ǯ��
               05  RECE61-AGE             PIC  9(03).
      *��������ʬ
               05  RECE61-SYUGAKUKBN      PIC 9(01).
      *�ƥ��ȴ����ֹ��ʬ
               05  RECE61-TSTPTNUMKBN     PIC  X(01).
      *�����͹���ֹ�
               05  RECE61-HOME-POST       PIC X(07).
      *����ݽ���
               05  RECE61-HOME-ADRS       PIC X(200).
      *�������������
               05  RECE61-HOME-BANTI      PIC X(200).
      *Ϣ�����̾��
               05  RECE61-RENRAKU-NAME    PIC X(100).
      *Ϣ�����͹���ֹ�
               05  RECE61-RENRAKU-POST    PIC X(07).
      *Ϣ����ݽ���
               05  RECE61-RENRAKU-ADRS    PIC X(200).
      *Ϣ�������������
               05  RECE61-RENRAKU-BANTI   PIC X(200).
      *Ϣ������������ֹ�
               05  RECE61-RENRAKU-TEL1    PIC X(15).
      *Ϣ������������ֹ�
               05  RECE61-RENRAKU-TEL2    PIC X(15).
      *���ֹ�
           03  RECE61-PREFNUM             PIC  9(02).
      *��������������ե饰
           03  RECE61-DOUJITSU-FLG        PIC 9(01).
      *����������
           03  RECE61-DOUJITSU-AREA.
      *Ʊ��������������ʬ�ˤ��ݸ��ȹ礻
               05  RECE61-DOUJITSU-HKNCOMBI
                                          PIC 9(04)   OCCURS  31.
      *Ʊ��������������ʬ�ˤο������ϻ��ֶ�ʬ
               05  RECE61-DOUJITSU-SKJKBN PIC X(01)   OCCURS  31.
      *������ʬ
               05  RECE61-NYUINDAY-TBL.
                   07  RECE61-NYUINDAY-KBN
                                          PIC  X(01)  OCCURS  31.
      *������
           03  RECE61-PRTSEQ              PIC  9(04).
      *̤����ե饰
           03  RECE61-MISEIKYU-FLG        PIC X(02).
      *̤������ʬ
           03  RECE61-TEISEIKBN           PIC  X(01).
      *�����쥻��������ե饰
           03  RECE61-TENKEN-FLG          PIC X(01).
      *�쥻�ץȼ����������顼����
           03  RECE61-ERR-AREA.
               05  RECE61-ERR-ERRKBN      PIC 9(01).
               05  RECE61-ERR-DAY         PIC 9(02).
               05  RECE61-ERR-HKNCOMBINUM PIC 9(04).
               05  RECE61-ERR-TEKSTYMD    PIC 9(08).
               05  RECE61-ERR-TEKEDYMD    PIC 9(08).
      