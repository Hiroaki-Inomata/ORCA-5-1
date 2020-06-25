       IDENTIFICATION              DIVISION.                            
       PROGRAM-ID.                 KOUZASUB.                            
       ENVIRONMENT                 DIVISION.                            
       CONFIGURATION               SECTION.                             
       INPUT-OUTPUT                SECTION.                             
       FILE-CONTROL.                                                    
      *    ̾�Υǡ���                                                   
           SELECT  KOUZA-FILE      ASSIGN  "/usr/KOUZA"                      
                                   ORGANIZATION IS LINE SEQUENTIAL      
                                   FILE    STATUS  IS  STS-KOUZA.       
       DATA                        DIVISION.                            
       FILE                        SECTION.                             
      *    ���¥ǡ���                                                   
       FD  KOUZA-FILE.                                                  
       01  KOUZA01-REC.                                                 
           02  KOUZA01-DATA        PIC X(80).                           
      *                                                                 
       WORKING-STORAGE             SECTION.                             
      *                                                                 
      *�ե����륹�ơ������ΰ�                                           
       01  STS-AREA.                                                    
           03  STS-KOUZA           PIC X(02).                           
      *                                                                 
      *�ե饰�ΰ�                                                       
       01  FLG-AREA.                                                    
           03  FLG-END             PIC 9(01)  VALUE  ZERO.              
      *                                                                 
      *����ǥå����ΰ�                                                 
       01  IDX-AREA.                                                    
           03  IDX-KOUZA           PIC 9(01)  VALUE  ZERO.              
      *                                                                 
      *                                                                 
       LINKAGE SECTION.                                                 
       01  WRK-PARA-OUT.                                                
           02  WRK-PARA-OUT-DATA   PIC X(80)  OCCURS 7.                 
      ******************************************************************
       PROCEDURE           DIVISION                                     
               USING                                                    
           WRK-PARA-OUT.                                                
      *                                                                 
      ******************************************************************
      *    �����                                                      *
      ******************************************************************
       000-PROC-SEC                SECTION.                             
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           EXIT  PROGRAM                                                
           .                                                            
      *                                                                 
      ******************************************************************
      *    �������                                                    *
      ******************************************************************
       100-INIT-SEC                SECTION.                             
           OPEN    INPUT               KOUZA-FILE                       
           MOVE    SPACE               TO  WRK-PARA-OUT.                
           .                                                            
       100-INIT-EXT.                                                    
           EXIT.                                                        
      *                                                                 
      ******************************************************************
      *    ��λ����                                                    *
      ******************************************************************
       300-END-SEC                 SECTION.                             
           CLOSE                       KOUZA-FILE                       
           .                                                            
       300-END-EXT.                                                     
           EXIT.                                                        
      *                                                                 
      ******************************************************************
      *    ���������������������������������������������������        *
      ******************************************************************
       200-MAIN-SEC                SECTION.                 
           PERFORM 2001-INREAD-SEC                                      
           PERFORM                      VARYING  IDX-KOUZA              
                                        FROM  1  BY  1                  
                                        UNTIL (IDX-KOUZA > 7 ) OR       
                                              (FLG-END   = 1 )          
               MOVE  KOUZA01-DATA      TO  WRK-PARA-OUT-DATA (IDX-KOUZA)
               PERFORM 2001-INREAD-SEC                                  
           END-PERFORM                                                  
           .                                                            
       200-MAIN-EXT.                                                    
           EXIT.                                                        
      *                                                                 
      ******************************************************************
      *    ���¥ե������ɹ���                                          *
      ******************************************************************
       2001-INREAD-SEC             SECTION.                             
           READ    KOUZA-FILE          AT  END                          
               MOVE    1               TO  FLG-END                      
           END-READ                                                     
           .                                                            
       2001-INERAD-EXT.                                                 
           EXIT.