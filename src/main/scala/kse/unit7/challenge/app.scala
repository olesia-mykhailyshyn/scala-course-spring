package kse.unit7.challenge

import kse.unit7.challenge.adt.*
import kse.unit7.challenge.model.*
import kse.unit7.challenge.services.*

object app:

  def getPostsViews(apiKey: ApiKey): Try[List[PostView]] =
    for
      userProfile <- getUserProfile(apiKey)
      posts       <- getPosts(userProfile.userId)
      postViews <- posts.foldLeft(Try(List.empty[PostView])) { (accTry, post) =>
        accTry flatMap { acc =>
          getPostView(post) map { view =>
            acc :+ view
          }
        }
      }
    yield postViews

  def getPostsViewDesugared(apiKey: ApiKey): Try[List[PostView]] =
    getUserProfile(apiKey) flatMap { userProfile =>
      getPosts(userProfile.userId) flatMap { posts =>
        posts.foldLeft(Try(List.empty[PostView])) { (accTry, post) =>
          accTry flatMap { acc =>
            getPostView(post) map { view =>
              acc :+ view
            }
          }
        }
      }
    }

  def getPostView(post: Post): Try[PostView] =
    for
      comments <- services.getComments(post.postId)
      likes    <- services.getLikes(post.postId)
      shares   <- services.getShares(post.postId)
    yield PostView(post, comments, likes, shares)

  def getPostViewDesugared(post: Post): Try[PostView] =
    services.getComments(post.postId).flatMap { comments =>
      services.getLikes(post.postId).flatMap { likes =>
        services.getShares(post.postId).map { shares =>
          PostView(post, comments, likes, shares)
        }
      }
    }
