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
      *         �����ƥ�����ޥ���     (CPSK2005.INC)            *
      *         �ʣ�������                                       *
      *         �쥻�ץ��Խ���ʬ                                 *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    MCC-ƣ��     02/02/26  �쥻�Ŷ�ʬ���쥻�Ž�������ɲ�
      *  01.00.02    MCC-ƣ��     02/03/28  ���ݣϣãҶ�ʬ�����ݿ����������
      *                                     ��ʬ���ɲ�
      *  01.00.03    MCC-ƣ��     02/04/03  ��ñ�ѹ����Խ��ץ������̾���ɲ�
      *  01.00.04    MCC-ƣ��     02/05/13  ��̾�Խ���ʬ���ʿ��Ų��̡ˤ��ɲ�
      *  01.00.05    MCC-ƣ��     02/06/04  ž�����Խ���ʬ���ɲ�
      *  01.00.06    MCC-ƣ��     02/07/04  ������ץ�󥿾�����ɲ�
      *  01.00.07    NACL-ƣ��    03/01/08  �̤��ֹ��Խ���ʬ���ɲ�
      *  01.00.08    NACL-ƣ��    03/04/09  ³���ʬ�������ˤ��ɲ�
      *  01.00.09    NACL-ƣ��    03/07/02  ����̾�Խ��ե饰���ɲ�
      *  01.00.10    NACL-ƣ��    03/08/21  ³���ʬ���������ˡ�³�������
      *                                     ���ɲ�
      *  01.00.11    NACL-����    03/11/18  Ϸ�ͤȸ�����ʬ�ɲ�
      *  01.00.12    NACL-ƣ��    04/02/18  ���ݣϣãҶ�ʬ���ɲ�
      *  01.00.13    NACL-ƣ��    04/04/09  �쥻�ץȥ����ȡʾɾ��ܵ��˵�Ͽ��ʬ
      *                                     �����Ų��Խ���ʬ���ɲ�
      *  01.00.14    NACL-ƣ��    04/07/02  �쥻�ץ����Ͻ������ʬ��
      *                                     ��ư���ܶ�ʬ���ɲ�
      *
      *  02.07.01    NACL-ƣ��    05/08/20  �����������б�
      *                                     �������������ʸ�����ܰ���
      *                                     ���ճ���󵭺ܶ�ʬ
      *                                     �쥻�ץȼ����󵭺ܶ�ʬ
      *  02.07.02    NACL-ƣ��    05/12/12  ���ݥ쥻�ץ����ֽ��б�
      *  
      *  02.09.01    NACL-ƣ��    06/05/12  �����̿��壱����������
      *                                     �쥻������ɽ�쥳���ɵ�Ͽ���
      *                                     ���ɲ�
      *  3.0.0
      *  03.00.01    NACL-ƣ��    06/05/15  ���ౡ����ε��ܶ�ʬ���ɲ�
      *
      *  03.02.01    NACL-ƣ��    06/07/11  ϫ�ҡ������դΥץ�󥿻�����ɲ�
      *  03.02.02    NACL-ƣ��    06/07/24  �ޥ�Ĺ���������б�
      *  03.02.03    NACL-ƣ��    06/07/26  �����դ��Ȱ�������ǽ�б�
      *
      *  03.04.01    NACL-ƣ��    07/01/16  �쥻�ץȰ��������Ͻ��ʬ
      *                                     �����Ρˤ��ɲ�
      *
      *  03.05.01    NACL-ƣ��    07/06/25  �����դ��ͼ�����ǽ
      *                                     ��ʿ������ǯ��������ͼ���
      *                                     �����쥻�������ֹ浭�ܶ�ʬ
      *                                     ����������Υ����å������̵ͭ
      *                                     ���ɲ� 
      *
      *  04.02.01    NACL-ƣ��    08/03/04  ʿ������ǯ��������б�
      ************************************************************
       01  SYS-2005-REC.
           03  SYS-2005-KEY.
               05  SYS-2005-HOSPNUM     PIC 9(2).
      *����������
               05  SYS-2005-KANRICD     PIC  X(04).
      *��ʬ������
               05  SYS-2005-KBNCD       PIC  X(08).
      *ͭ������ǯ����
               05  SYS-2005-STYUKYMD    PIC  X(08).
      *ͭ����λǯ����
               05  SYS-2005-EDYUKYMD    PIC  X(08).
      *�����ơ��֥�
           03  SYS-2005-TBL.
      *��̾�Խ���ʬ
               05  SYS-2005-BYOMEIHENKBN
                                        PIC  X(01).
      *���ݥ쥻�Ž��϶�ʬ
               05  SYS-2005-KKHRECEKBN  PIC  X(01).
      *���ݥ쥻�Ž��϶�ʬ
               05  SYS-2005-SYARECEKBN  PIC  X(01).
      *�쥻�ťǡ���������
               05  SYS-2005-RECEDENOUT  PIC  X(30).
      *���ݣϣãҽ��϶�ʬ
               05  SYS-2005-KKHOCRKBN   PIC  X(01).
      *���ݿ�����������ʬ
               05  SYS-2005-KKHPRTKBN   PIC  X(01).
      *��ñ�ѥץ������̾
               05  SYS-2005-PREFPRGID-G.
                   07  SYS-2005-PREFPRGID    OCCURS  10
                                        PIC  X(15).
      *��̾�Խ���ʬ��
               05  SYS-2005-BYOMEIHENKBN1
                                        PIC  X(01).
      *ž�����Խ���ʬ
               05  SYS-2005-TENKIHENKBN PIC  X(01).
      *���ݽ�����ץ��̾
               05  SYS-2005-SYAPRTNM    PIC  X(16).
      *���ݽ�����ץ��̾
               05  SYS-2005-KKHPRTNM    PIC  X(16).
      *�̤��ֹ��Խ���ʬ
               05  SYS-2005-RENNUMHENKBN
                                        PIC  X(01).
      *³���ʬ��������
               05  SYS-2005-NEXTPRTKBN  PIC  X(01).
      *����̾�Խ��ե饰
               05  SYS-2005-SYUBYOHENFLG
                                        PIC  X(01).
      *³���ʬ����������
               05  SYS-2005-NEXTPRTKBN-G
                                        PIC  X(01).
      *���ݽ�����ץ��̾��³���
               05  SYS-2005-SYAPRTNM-NEXT
                                        PIC  X(16).
      *���ݽ�����ץ��̾��³���
               05  SYS-2005-KKHPRTNM-NEXT
                                        PIC  X(16).
      *Ϸ�ͤȸ�����ʬ
               05  SYS-2005-RJNGNBKKBN  PIC  X(01).
      *����쥻Ŧ���󵭺ܶ�ʬ
               05  SYS-2005-JIHITEKKSIKBN
                                        PIC  X(01).
      *���ݣϣãҽ��϶�ʬ
               05  SYS-2005-SYAOCRKBN   PIC  X(01).
      *���Ų��Խ���ʬ
               05  SYS-2005-SRYKAHENKBN PIC  X(01).
      *�쥻�ץȥ����ȡʾɾ��ܵ��˵�Ͽ��ʬ
               05  SYS-2005-RECECOMKBN  PIC  X(01).
      *���ݽ�����ץ��̾��������
               05  SYS-2005-SYAPRTNM-N  PIC  X(16).
      *���ݽ�����ץ��̾��������
               05  SYS-2005-KKHPRTNM-N  PIC  X(16).
      *���ݽ�����ץ��̾��³��ˡ�������
               05  SYS-2005-SYAPRTNM-NEXT-N
                                        PIC  X(16).
      *���ݽ�����ץ��̾��³��ˡ�������
               05  SYS-2005-KKHPRTNM-NEXT-N
                                        PIC  X(16).
      *��ư���ܶ�ʬ
               05  SYS-2005-AUTODSPKBN  PIC  X(01).
      *�쥻�ץȰ��������Ͻ��ʬ�ʼ��ݡ�
               05  SYS-2005-PRTJYUNKBN1 PIC  X(02).
      *�쥻�ץȸ��̺������Ͻ��ʬ
               05  SYS-2005-PRTJYUNKBN2 PIC  X(02).
      *�������������ʸ�����ܰ���
               05  SYS-2005-MOJI-KISAIKBN
                                        PIC  X(01).
      *���ճ���󵭺ܶ�ʬ
               05  SYS-2005-KYURATE-KISAIKBN
                                        PIC  X(01).
      *�쥻�ץȼ����󵭺ܶ�ʬ
               05  SYS-2005-RECESYUBETU-KISAIKBN
                                        PIC  X(01).
      *�쥻�ץȰ��������Ͻ��ʬ�ʹ��ݡ�
               05  SYS-2005-PRTJYUNKBN3 PIC  X(02).
      *�쥻������ɽ�쥳���ɵ�Ͽ���
               05  SYS-2005-NI-JYOKEN-AREA.  
                   07  SYS-2005-NI-JYOKEN
                                        PIC  9(08)   OCCURS  4.
      *���ౡ���򵭺ܶ�ʬ���������쥻�ץȤΤߡ�
               05  SYS-2005-NYUTAIINREKI-KSIKBN
                                        PIC  X(01).
      *ϫ�ҽ�����ץ��̾
               05  SYS-2005-ROUPRTNM    PIC  X(16).
      *ϫ�ҽ�����ץ��̾��³���
               05  SYS-2005-ROUPRTNM-NEXT
                                        PIC  X(16).
      *ϫ�ҽ�����ץ��̾��������
               05  SYS-2005-ROUPRTNM-N  PIC  X(16).
      *ϫ�ҽ�����ץ��̾��³��ˡ�������
               05  SYS-2005-ROUPRTNM-NEXT-N
                                        PIC  X(16).
      *�����ս�����ץ��̾
               05  SYS-2005-JBAIPRTNM   PIC  X(16).
      *�����ս�����ץ��̾��³���
               05  SYS-2005-JBAIPRTNM-NEXT
                                        PIC  X(16).
      *�����ս�����ץ��̾��������
               05  SYS-2005-JBAIPRTNM-N PIC  X(16).
      *�����ս�����ץ��̾��³��ˡ�������
               05  SYS-2005-JBAIPRTNM-NEXT-N
                                        PIC  X(16).
      *�ȵ��ܶ�ʬ�ʼ����ա���������
               05  SYS-2005-FRAME-JIBAIPRTKBN
                                        PIC  X(01).
      *�ȵ��ܶ�ʬ�ʼ����ա�������
               05  SYS-2005-FRAME-JIBAIPRTKBN-N
                                        PIC  X(01).
      *�����������ޥ�Ĺʬ������ʬ�ʼ��ݡ�
               05  SYS-2005-MARUCHO-SYAPRTKBN
                                        PIC  X(01).
      *�쥻�ץȰ��������Ͻ��ʬ�����Ρ�
               05  SYS-2005-PRTJYUNKBN4 PIC  X(02).
      *ü���ɣ�
           03  SYS-2005-TERMID          PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SYS-2005-OPID            PIC  X(16).
      *����ǯ����
           03  SYS-2005-CREYMD          PIC  X(08).
      *����ǯ����
           03  SYS-2005-UPYMD           PIC  X(08).
      *��������
           03  SYS-2005-UPHMS           PIC  X(06).
      *�祭�������ѹ���
           03  SYS-2005-UPD-KEY.
               05  SYS-2005-UPD-HOSPNUM     PIC 9(2).
               05  SYS-2005-UPD-KANRICD     PIC X(04).
               05  SYS-2005-UPD-KBNCD       PIC X(08).
               05  SYS-2005-UPD-STYUKYMD    PIC X(08).
               05  SYS-2005-UPD-EDYUKYMD    PIC X(08).