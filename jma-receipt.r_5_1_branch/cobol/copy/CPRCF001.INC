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
      *    �쥻�ץ����ٽ񡦰����������   COPY (CPRCF001.INC)    *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    MCC-ƣ��     01.06.27  ��³��ʬ���ɲ�
      *  01.00.02    MCC-ƣ��     01.06.29  Ŭ�ѽ�λǯ�������ɲ�
      *  01.01.03    MCC-ƣ��     02.02.01  ��­��ʬ���ɲ�
      *  01.01.04    MCC-ƣ��     02.02.13  �������ݸ��ɣĤ���
      *                                     �ݸ��ֹ���ѹ�
      *                                     �ܿͲ�²��ʬ���ɲ�
      *                                     �ݸ��ȹ礻�򣳤��飱�����ѹ�
      *  01.01.05    MCC-ƣ��     02.03.13  Ŭ��ǯ������ʣ���Ĥ��ѹ�
      *  01.01.06    MCC-ƣ��     02.05.24  �ݸ��ѹ�����̤�����ե饰���ɲ�
      *  01.01.07    NACL-ƣ��    03.02.12  �����ʬ���ɲ�
      *  01.01.08    NACL-ƣ��    03.03.03  ���Ųʿ��������飲�����ѹ�
      *  01.01.09    NACL-ƣ��    03.03.11  �쥻�ץȼ�������������ɲ�
      *  01.01.10    NACL-ƣ��    03.10.27  ���Ծ�����ɲ�
      *                                     ���ֹ���ɲ�
      *  01.01.11    NACL-ƣ��    04.06.15  ��������ɲáʸ��̺������Τߡ�
      *  01.01.12    NACL-ƣ��    04.11.01  ����쥻�ץ������ʬ�����ݸ�
      *                                     ������ɲ�
      *
      *  ver.2.6.0 
      *  02.06.01    NACL-ƣ��    05.07.13  �ݸ��ȹ礻���ɲ�
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
      *
      *  04.06.01    NACL-ƣ��    10/10/12  ����ñ�ȤΥ쥻ʬΥ�б�
      *                                     ������kohid �ɲ�
      *  04.06.02    NACL-ƣ��    11/12/12  �����쥻������������б�
      *  04.06.03    NACL-ƣ��    12/10/04  Ʊ��쥻�ץȤθ��񤬣��Ĥ�Ķ
      *                                     �������μ��������б�
      *  04.06.04    NACL-ƣ��    12/10/26  �õ����ࣰ����Ϸʻ�˵ڤ�
      *                                     ������Ϸ��ˤΥ쥻�ץ��б�
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
      ************************************************************
      *01  RECE01-REC.
           03  RECE01-KEY.
      *���ŵ��أɣ�
               05  RECE01-HOSPNUM         PIC 9(2).
      *����ǯ��
               05  RECE01-SRYYM           PIC  9(06).
      *������ʬ
               05  RECE01-NYUGAIKBN       PIC  X(01).
      *���ԣɣ�
               05  RECE01-PTID            PIC  9(10).
      *ʬ���ʬ
               05  RECE01-RECEIPT-KBN     PIC  X(10).
      *�쥻�ץȿ��Ų�
               05  RECE01-RECEKA          PIC  X(02).
      *���ݸ��ݸ��ֹ�
               05  RECE01-HKNNUM          PIC  X(03).
      *����ɣ�
               05  RECE01-KOHID           PIC  9(10).
      *�ݸ����ֹ�        
               05  RECE01-HKNJANUM        PIC  X(08).
      *�ܿͲ�²��ʬ
               05  RECE01-HONKZKKBN       PIC  X(01).
      *�����ʬ
               05  RECE01-HOJOKBN         PIC  X(01).
      *����ɣġʸ���ñ�Ȼ����ѡ�
               05  RECE01-KOH-KOHID       PIC  9(10).
      *Ŭ�ѳ���ǯ����
               05  RECE01-TEKSTYMD        PIC  X(08). 
      *�����ΰ�
           03 RECE01-KOHI-AREA.
               05  RECE01-KOHI-TBL        OCCURS  10.
      *    ����ɣ�
                   07  RECE01-KOHID-O     PIC  9(10).
      *    �����ݸ��ֹ�
                   07  RECE01-KOHHKNNUM-O PIC  X(03).
      *    �����ʧ��ʬ
                   07  RECE01-KOHPAYKBN-O PIC  X(02).
      *    ����쥻�ץ������ʬ�ʶ��򡧥쥻���ܤΤߡ��������ܤʤ���
                   07  RECE01-RECESKYKBN-O
                                          PIC  X(01).
      *    ����Ŭ�ѳ���ǯ����
                   07  RECE01-KOHTEKSTYMD-O
                                          PIC  X(08).
      *    ˡ���ֹ�
                   07  RECE01-KOHHBTNUM-O PIC  X(02).
      *    ������ô���ֹ�
                   07  RECE01-KOHFTNJANUM-O
                                          PIC  X(08).
      *    ���������ֹ�
                   07  RECE01-KOHJKYSNUM-O
                                          PIC  X(20).
      *
      *���ݸ�����
           03  RECE01-HKN.
      *���ݸ����ݸ��ɣ�
               05  RECE01-HKNID           PIC  9(10).
      *���ݸ���ˡ���ֹ�
               05  RECE01-HKNHBTNUM       PIC  X(02).
      *���ݸ����ݸ����ֹ�
               05  RECE01-HKN-HKNJANUM    PIC  X(08).
      *���ݸ����ܿͲ�²��ʬ
               05  RECE01-HKN-HONKZKKBN   PIC  X(01).
      *���ݸ��������ʬ
               05  RECE01-HKN-HOJOKBN     PIC  X(01).
      *���ݸ��ݷ�³��ʬ
               05  RECE01-CONTKBN         PIC  X(01).
      *���ݸ��ݵ���
               05  RECE01-KIGO            PIC  X(80).
      *���ݸ����ֹ�
               05  RECE01-NUM             PIC  X(80).
      *���ݸ��ݾ������ֹ�
               05  RECE01-CERTNUM         PIC  X(03).
      *
      *�ݸ��ȹ礻�ֹ��ΰ�
           03 RECE01-HKNCOMBI-AREA.
               05  RECE01-HKNCOMBI-TBL    OCCURS  10.
      *�ݸ��ȹ礻�ֹ�
                   07  RECE01-HKNCOMBI-O  PIC  9(04).
      *���ݸ��ֹ�
                   07  RECE01-HKNNUMHC    PIC  X(03).
      *�ȹ礻�����ݸ��ֹ����
                   07  RECE01-KOHNUMHCINF OCCURS  4.
      *�ȹ礻�����ݸ��ֹ�
                       09  RECE01-KOHNUMHC
                                          PIC  X(03).
      *�ȹ礻�����ݸ��ɣ�
                       09  RECE01-KOHIDHC PIC  9(10).
      *
      *���Ųʾ���
           03 RECE01-SRYKAINF.
               05  RECE01-SRYKA           PIC  X(02)  OCCURS  20.
      *Ŭ��ǯ�����ΰ�
           03 RECE01-TEKYMD-AREA.
               05  RECE01-TEKYMD-TBL      OCCURS  10.
      *Ŭ�ѳ���ǯ����
                   07  RECE01-TEKSTYMDX   PIC  X(08). 
      *Ŭ�ѽ�λǯ����
                   07  RECE01-TEKEDYMDX   PIC  X(08). 
      *
      *�쥻�ץ����ᤷ�ʤ������ΰ�
           03 RECE01-KOHSKY-KOHI-AREA.
               05  RECE01-KOHSKY-KOHI-TBL OCCURS  4.
      *    ����ɣ�
                   07  RECE01-KOHSKY-KOHID-O
                                          PIC  9(10).
      *    �����ݸ��ֹ�
                   07  RECE01-KOHSKY-KOHHKNNUM-O
                                          PIC  X(03).
      *���Ծ���
           03  RECE01-PTINF.
      *�����ֹ�  
               05  RECE01-PTNUM           PIC  X(20).
      *���ʻ�̾
               05  RECE01-KANANAME        PIC  X(50).
      *������̾
               05  RECE01-NAME            PIC  X(100).
      *����
               05  RECE01-SEX             PIC  X(01).
      *��ǯ����
               05  RECE01-BIRTHDAY        PIC  9(08).
      *��˴��ʬ
               05  RECE01-DEATHKBN        PIC  X(01).
      *ǯ��
               05  RECE01-AGE             PIC  9(03).
      *��������ʬ
               05  RECE01-SYUGAKUKBN      PIC 9(01).
      *�ƥ��ȴ����ֹ��ʬ
               05  RECE01-TSTPTNUMKBN     PIC  X(01).
      *�����͹���ֹ�
               05  RECE01-HOME-POST       PIC X(07).
      *����ݽ���
               05  RECE01-HOME-ADRS       PIC X(200).
      *�������������
               05  RECE01-HOME-BANTI      PIC X(200).
      *Ϣ�����̾��
               05  RECE01-RENRAKU-NAME    PIC X(100).
      *Ϣ�����͹���ֹ�
               05  RECE01-RENRAKU-POST    PIC X(07).
      *Ϣ����ݽ���
               05  RECE01-RENRAKU-ADRS    PIC X(200).
      *Ϣ�������������
               05  RECE01-RENRAKU-BANTI   PIC X(200).
      *Ϣ������������ֹ�
               05  RECE01-RENRAKU-TEL1    PIC X(15).
      *Ϣ������������ֹ�
               05  RECE01-RENRAKU-TEL2    PIC X(15).
      *���ֹ�
           03  RECE01-PREFNUM             PIC  9(02).
      *������
           03  RECE01-PRTSEQ              PIC  9(04).
      *̤����ե饰
           03  RECE01-MISEIKYU-FLG        PIC X(02).
      *̤������ʬ
           03  RECE01-TEISEIKBN           PIC  X(01).
      *�����쥻��������ե饰
           03  RECE01-TENKEN-FLG          PIC X(01).
      *�쥻�ץȼ����������顼����
           03  RECE01-ERR-AREA.
               05  RECE01-ERR-ERRKBN      PIC 9(01).
               05  RECE01-ERR-DAY         PIC 9(02).
               05  RECE01-ERR-HKNCOMBINUM PIC 9(04).
               05  RECE01-ERR-TEKSTYMD    PIC 9(08).
               05  RECE01-ERR-TEKEDYMD    PIC 9(08).