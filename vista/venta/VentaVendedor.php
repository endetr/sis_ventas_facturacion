<?php
/**
*@package pXP
*@file gen-SistemaDist.php
*@author  (rarteaga)
*@date 20-09-2011 10:22:05
*@description Archivo con la interfaz de usuario que permite la ejecucion de todas las funcionalidades del sistema
*/
header("content-type: text/javascript; charset=UTF-8");
?>
<script>
Phx.vista.VentaVendedor = {    
    bsave:false,    
    require:'../../../sis_ventas_farmacia/vista/venta/Venta.php',
    requireclase:'Phx.vista.Venta',
    title:'Venta',
    nombreVista: 'VentaVendedor',
    
    constructor: function(config) {
        this.maestro=config.maestro;  
        Phx.vista.VentaVendedor.superclass.constructor.call(this,config);
        
        this.store.baseParams.pes_estado = 'borrador';
        this.load({params:{start:0, limit:this.tam_pag}});
        
        this.finCons = true;
        this.addButton('ant_estado',{grupo:[3,4],argument: {estado: 'anterior'},text:'Anterior',iconCls: 'batras',disabled:true,handler:this.antEstado,tooltip: '<b>Pasar al Anterior Estado</b>'});
        this.addButton('sig_estado',{grupo:[0,2],text:'Siguiente',iconCls: 'badelante',disabled:true,handler:this.sigEstado,tooltip: '<b>Pasar al Siguiente Estado</b>'});
        this.addButton('diagrama_gantt',{grupo:[0,1,2,3,4],text:'Gant',iconCls: 'bgantt',disabled:true,handler:this.diagramGantt,tooltip: '<b>Diagrama Gantt de la venta</b>'});
        this.addButton('btnImprimir',
            {   grupo:[0,1,2,3,4],
                text: 'Imprimir',
                iconCls: 'bpdf32',
                disabled: true,
                handler: this.imprimirNota,
                tooltip: '<b>Imprimir Formulario de Venta</b><br/>Imprime el formulario de la venta'
            }
        );
        
    } ,
    gruposBarraTareas:[{name:'borrador',title:'<H1 align="center"><i class="fa fa-eye"></i> En Registro</h1>',grupo:0,height:0},
                       {name:'proceso_elaboracion',title:'<H1 align="center"><i class="fa fa-eye"></i> En elaboración</h1>',grupo:1,height:0},
                       {name:'pendiente_entrega',title:'<H1 align="center"><i class="fa fa-eye"></i> Para Entrega</h1>',grupo:2,height:0},
                       {name:'entregado',title:'<H1 align="center"><i class="fa fa-eye"></i> Entregado</h1>',grupo:3,height:0},
                       {name:'descartado',title:'<H1 align="center"><i class="fa fa-eye"></i> Descartado</h1>',grupo:4,height:0}],
    
    actualizarSegunTab: function(name, indice){
        if(this.finCons){
             this.store.baseParams.pes_estado = name;
             this.load({params:{start:0, limit:this.tam_pag}});
           }
    },
    beditGroups: [0],
    bdelGroups:  [0],
    bactGroups:  [0,1,2,3,4],
    btestGroups: [0],
    bexcelGroups: [0,1,2,3,4],       
    preparaMenu:function()
    {   var rec = this.sm.getSelected();
        
        if (rec.data.estado == 'borrador') {
              this.getBoton('ant_estado').disable();
              this.getBoton('sig_estado').enable();
                          
        } else {
             this.getBoton('ant_estado').enable();
             this.getBoton('sig_estado').enable();
        }
               
        this.getBoton('diagrama_gantt').enable(); 
        Phx.vista.VentaVendedor.superclass.preparaMenu.call(this);
    },
    liberaMenu:function()
    {   
        this.getBoton('diagrama_gantt').disable();
        this.getBoton('ant_estado').disable();
        this.getBoton('sig_estado').disable();        
        Phx.vista.VentaVendedor.superclass.liberaMenu.call(this);
    },
    onButtonNew : function () {
        //abrir formulario de solicitud
           var me = this;
           me.objSolForm = Phx.CP.loadWindows('../../../sis_ventas_farmacia/vista/venta/FormVenta.php',
                                    'Formulario de Venta',
                                    {
                                        modal:true,
                                        width:'80%',
                                        height:'90%'
                                    }, {data:{objPadre: me}
                                    }, 
                                    this.idContenedor,
                                    'FormVenta',
                                    {
                                        config:[{
                                                  event:'successsave',
                                                  delegate: this.onSaveForm,
                                                  
                                                }],
                                        
                                        scope:this
                                     });      
    }    
    
};
</script>