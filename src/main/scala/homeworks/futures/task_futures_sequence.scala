package homeworks.futures

import homeworks.HomeworksUtils.TaskSyntax

import scala.concurrent.{ExecutionContext, Future}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {

    futures.foldLeft[Future[(List[A], List[Throwable])]](Future.successful(List.empty[A], List.empty[Throwable])) {
      (future, elem) => {
        future.zipWith(elem)((f,v)=> (f._1 :+ v,f._2)).recoverWith{
          case e:Exception => future.map(data => (data._1, data._2:+ e))
        }
      }
    }
  }
}
